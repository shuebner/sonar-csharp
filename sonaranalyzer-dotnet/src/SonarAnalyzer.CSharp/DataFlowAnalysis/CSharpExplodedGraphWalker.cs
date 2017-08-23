/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2017 SonarSource SA
 * mailto: contact AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.DataFlowAnalysis.CSharp
{
    internal class CSharpExplodedGraphWalker : AbstractExplodedGraphWalker
    {
        private readonly IEnumerable<ConstraintDecorator> decorators;
        private readonly CSharpInstructionVisitor instructionVisitor;

        protected override IEnumerable<ConstraintDecorator> ConstraintDecorators => decorators;

        public CSharpExplodedGraphWalker(IControlFlowGraph cfg, ISymbol declaration, SemanticModel semanticModel,
            LiveVariableAnalysis lva)
            : base(cfg, declaration, semanticModel, lva)
        {
            instructionVisitor = new CSharpInstructionVisitor(this);

            decorators = ImmutableList.Create<ConstraintDecorator>(
                new ObjectConstraintDecorator(this),
                new BooleanConstraintDecorator(this),
                new NullableConstraintDecorator(this),
                new CollectionConstraintDecorator(this),
                new DisposableConstraintDecorator(this));
        }

        protected override void VisitSimpleBlock(SimpleBlock block, ExplodedGraphNode node)
        {
            var newProgramState = node.ProgramState;

            var usingFinalizerBlock = block as UsingEndBlock;
            if (usingFinalizerBlock != null)
            {
                // TODO: using block
                //newProgramState = InvokeChecks(newProgramState, (ps, check) => check.PreProcessUsingStatement(node.ProgramPoint, ps));
                newProgramState = CleanStateAfterBlock(newProgramState, block);
                EnqueueAllSuccessors(block, newProgramState);
                return;
            }

            newProgramState = CleanStateAfterBlock(newProgramState, block);

            if (block is ForeachCollectionProducerBlock)
            {
                newProgramState = newProgramState.PopValue();
                EnqueueAllSuccessors(block, newProgramState);
                return;
            }

            var forInitializerBlock = block as ForInitializerBlock;
            if (forInitializerBlock != null)
            {
                newProgramState = newProgramState
                    .PopValues(forInitializerBlock.ForNode.Initializers.Count)
                    .PushValues(Enumerable.Range(0, forInitializerBlock.ForNode.Incrementors.Count)
                                          .Select(i => new SymbolicValue()));

                EnqueueAllSuccessors(forInitializerBlock, newProgramState);
                return;
            }

            var lockBlock = block as LockBlock;
            if (lockBlock != null)
            {
                newProgramState = newProgramState
                    .PopValue()
                    .RemoveSymbols(IsFieldSymbol);

                EnqueueAllSuccessors(block, newProgramState);
                return;
            }

            base.VisitSimpleBlock(block, node);
        }

        protected override void VisitBinaryBranch(BinaryBranchBlock binaryBranchBlock, ExplodedGraphNode node)
        {
            var newProgramState = CleanStateAfterBlock(node.ProgramState, node.ProgramPoint.Block);

            switch (binaryBranchBlock.BranchingNode.Kind())
            {
                case SyntaxKind.ForEachStatement:
                    VisitForeachBinaryBranch(binaryBranchBlock, newProgramState);
                    return;
                case SyntaxKind.CoalesceExpression:
                    VisitCoalesceExpressionBinaryBranch(binaryBranchBlock, newProgramState);
                    return;
                case SyntaxKind.ConditionalAccessExpression:
                    VisitConditionalAccessBinaryBranch(binaryBranchBlock, newProgramState);
                    return;

                case SyntaxKind.LogicalAndExpression:
                case SyntaxKind.LogicalOrExpression:
                    VisitBinaryBranch(binaryBranchBlock, node,
                        ((BinaryExpressionSyntax)binaryBranchBlock.BranchingNode).Left);
                    return;

                case SyntaxKind.WhileStatement:
                    VisitBinaryBranch(binaryBranchBlock, node,
                        ((WhileStatementSyntax)binaryBranchBlock.BranchingNode).Condition);
                    return;
                case SyntaxKind.DoStatement:
                    VisitBinaryBranch(binaryBranchBlock, node,
                        ((DoStatementSyntax)binaryBranchBlock.BranchingNode).Condition);
                    return;
                case SyntaxKind.ForStatement:
                    VisitBinaryBranch(binaryBranchBlock, node,
                        ((ForStatementSyntax)binaryBranchBlock.BranchingNode).Condition);
                    return;

                case SyntaxKind.IfStatement:
                    VisitBinaryBranch(binaryBranchBlock, node,
                        ((IfStatementSyntax)binaryBranchBlock.BranchingNode).Condition);
                    return;
                case SyntaxKind.ConditionalExpression:
                    VisitBinaryBranch(binaryBranchBlock, node,
                        ((ConditionalExpressionSyntax)binaryBranchBlock.BranchingNode).Condition);
                    return;

                default:
                    Debug.Fail($"Branch kind '{binaryBranchBlock.BranchingNode.Kind()}' not handled");
                    VisitBinaryBranch(binaryBranchBlock, node, null);
                    return;
            }
        }

        protected override void VisitInstruction(ExplodedGraphNode node)
        {
            var instruction = node.ProgramPoint.Block.Instructions[node.ProgramPoint.Offset];
            var expression = instruction as ExpressionSyntax;
            var parenthesizedExpression = expression?.GetSelfOrTopParenthesizedExpression();
            var newProgramPoint = new ProgramPoint(node.ProgramPoint.Block, node.ProgramPoint.Offset + 1);
            var newProgramState = node.ProgramState;

            newProgramState = ConstraintDecorators.Aggregate(newProgramState,
                (ps, decorator) => decorator.PreProcessInstruction(node, ps));

            newProgramState = instructionVisitor.VisitInstruction(instruction, newProgramState);

            newProgramState = ConstraintDecorators.Aggregate(newProgramState,
                (ps, decorator) => decorator.PostProcessInstruction(node, ps));

            newProgramState = EnsureStackState(parenthesizedExpression, newProgramState);

            OnInstructionProcessed(instruction, node.ProgramPoint, newProgramState);

            EnqueueNewNode(newProgramPoint, newProgramState);
        }

        public ProgramState EnsureStackState(ExpressionSyntax parenthesizedExpression, ProgramState programState)
        {
            if (ShouldConsumeValue(parenthesizedExpression))
            {
                var newProgramState = programState.PopValue();
                Debug.Assert(!newProgramState.HasValue);

                return newProgramState;
            }

            return programState;
        }

        private void VisitForeachBinaryBranch(BinaryBranchBlock binaryBranchBlock, ProgramState programState)
        {
            // foreach variable is not a VariableDeclarator, so we need to assign a value to it
            var foreachVariableSymbol = SemanticModel.GetDeclaredSymbol(binaryBranchBlock.BranchingNode);
            var sv = new SymbolicValue();
            var newProgramState = instructionVisitor.StoreSymbolicValueIfSymbolIsTracked(foreachVariableSymbol, sv, programState);

            EnqueueAllSuccessors(binaryBranchBlock, newProgramState);
        }

        private void VisitCoalesceExpressionBinaryBranch(BinaryBranchBlock binaryBranchBlock, ProgramState programState)
        {
            SymbolicValue sv;
            var ps = programState.PopValue(out sv);

            foreach (var newProgramState in sv.TrySetConstraint(ObjectConstraint.Null, ps))
            {
                EnqueueNewNode(new ProgramPoint(binaryBranchBlock.TrueSuccessorBlock), newProgramState);
            }

            foreach (var newProgramState in sv.TrySetConstraint(ObjectConstraint.NotNull, ps))
            {
                var nps = newProgramState;

                if (!ShouldConsumeValue((BinaryExpressionSyntax)binaryBranchBlock.BranchingNode))
                {
                    nps = nps.PushValue(sv);
                }
                EnqueueNewNode(new ProgramPoint(binaryBranchBlock.FalseSuccessorBlock), nps);
            }
        }

        private void VisitConditionalAccessBinaryBranch(BinaryBranchBlock binaryBranchBlock, ProgramState programState)
        {
            SymbolicValue sv;
            var ps = programState.PopValue(out sv);

            foreach (var newProgramState in sv.TrySetConstraint(ObjectConstraint.Null, ps))
            {
                var nps = newProgramState;

                if (!ShouldConsumeValue((ConditionalAccessExpressionSyntax)binaryBranchBlock.BranchingNode))
                {
                    nps = nps.PushValue(SymbolicValue.Null);
                }
                EnqueueNewNode(new ProgramPoint(binaryBranchBlock.TrueSuccessorBlock), nps);
            }

            foreach (var newProgramState in sv.TrySetConstraint(ObjectConstraint.NotNull, ps))
            {
                EnqueueNewNode(new ProgramPoint(binaryBranchBlock.FalseSuccessorBlock), newProgramState);
            }
        }

        private void VisitBinaryBranch(BinaryBranchBlock binaryBranchBlock, ExplodedGraphNode node, SyntaxNode instruction)
        {
            var ps = node.ProgramState;
            SymbolicValue sv;

            var forStatement = binaryBranchBlock.BranchingNode as ForStatementSyntax;
            if (forStatement != null)
            {
                if (forStatement.Condition == null)
                {
                    ps = ps.PushValue(SymbolicValue.True);
                }
                ps = ps.PopValue(out sv);
                ps = ps.PopValues(forStatement.Incrementors.Count);
            }
            else
            {
                ps = ps.PopValue(out sv);
            }

            foreach (var newProgramState in sv.TrySetConstraint(BoolConstraint.True, ps))
            {
                OnConditionEvaluated(instruction, evaluationValue: true);

                var nps = binaryBranchBlock.BranchingNode.IsKind(SyntaxKind.LogicalOrExpression)
                    ? newProgramState.PushValue(SymbolicValue.True)
                    : newProgramState;

                EnqueueNewNode(new ProgramPoint(binaryBranchBlock.TrueSuccessorBlock),
                    CleanStateAfterBlock(nps, node.ProgramPoint.Block));
            }

            foreach (var newProgramState in sv.TrySetConstraint(BoolConstraint.False, ps))
            {
                OnConditionEvaluated(instruction, evaluationValue: false);

                var nps = binaryBranchBlock.BranchingNode.IsKind(SyntaxKind.LogicalAndExpression)
                    ? newProgramState.PushValue(SymbolicValue.False)
                    : newProgramState;

                EnqueueNewNode(new ProgramPoint(binaryBranchBlock.FalseSuccessorBlock),
                    CleanStateAfterBlock(nps, node.ProgramPoint.Block));
            }
        }

        protected override bool IsValueConsumingStatement(SyntaxNode jumpNode)
        {
            if (jumpNode.IsKind(SyntaxKind.LockStatement))
            {
                return true;
            }

            var usingStatement = jumpNode as UsingStatementSyntax;
            if (usingStatement != null)
            {
                return usingStatement.Expression != null;
            }

            var throwStatement = jumpNode as ThrowStatementSyntax;
            if (throwStatement != null)
            {
                return throwStatement.Expression != null;
            }

            var returnStatement = jumpNode as ReturnStatementSyntax;
            if (returnStatement != null)
            {
                return returnStatement.Expression != null;
            }

            var switchStatement = jumpNode as SwitchStatementSyntax;
            if (switchStatement != null)
            {
                return switchStatement.Expression != null;
            }

            // goto is not putting the expression to the CFG

            return false;
        }

        public static bool ShouldConsumeValue(ExpressionSyntax expression)
        {
            if (expression == null)
            {
                return false;
            }

            var parent = expression.Parent;
            var conditionalAccess = parent as ConditionalAccessExpressionSyntax;
            if (conditionalAccess != null &&
                conditionalAccess.WhenNotNull == expression)
            {
                return ShouldConsumeValue(conditionalAccess.GetSelfOrTopParenthesizedExpression());
            }

            return parent is ExpressionStatementSyntax ||
                parent is YieldStatementSyntax;
        }
    }
}
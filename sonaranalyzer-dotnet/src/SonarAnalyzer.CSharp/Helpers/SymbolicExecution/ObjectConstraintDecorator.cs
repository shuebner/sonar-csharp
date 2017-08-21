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

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis.CSharp
{
    internal class ObjectConstraintDecorator : ConstraintDecorator
    {
        public ObjectConstraintDecorator(CSharpExplodedGraphWalker explodedGraphWalker)
            : base(explodedGraphWalker)
        {
        }

        public override ProgramState PostProcessDeclarationParameters(IParameterSymbol symbol,
            SymbolicValue symbolicValue, ProgramState programState)
            => SetNonNullConstraintIfValueType(symbol, symbolicValue, programState);

        public override ProgramState PostProcessInstruction(ExplodedGraphNode node, ProgramState programState)
        {
            var newProgramState = programState;

            switch (node.Instruction.Kind())
            {
                case SyntaxKind.NullLiteralExpression:
                    break; // Constant literal expressions are already handled by exploded graph walker

                case SyntaxKind.IdentifierName:
                    {
                        var symbol = SemanticModel.GetSymbolInfo(node.Instruction).Symbol;

                        if (ExplodedGraphWalker.IsSymbolTracked(symbol))
                        {
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.PostIncrementExpression:
                case SyntaxKind.PostDecrementExpression:
                case SyntaxKind.PreIncrementExpression:
                case SyntaxKind.PreDecrementExpression:
                    {
                        var operand = node.Instruction is PostfixUnaryExpressionSyntax
                            ? ((PostfixUnaryExpressionSyntax)node.Instruction).Operand
                            : ((PrefixUnaryExpressionSyntax)node.Instruction).Operand;
                        var symbol = SemanticModel.GetSymbolInfo(operand).Symbol;

                        if (ExplodedGraphWalker.IsSymbolTracked(symbol))
                        {
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.VariableDeclarator:
                    {
                        var varDeclarator = (VariableDeclaratorSyntax)node.Instruction;
                        var symbol = SemanticModel.GetDeclaredSymbol(node.Instruction);

                        if (varDeclarator.Initializer?.Value != null &&
                            ExplodedGraphWalker.IsSymbolTracked(symbol))
                        {
                            var sv = node.ProgramState.ExpressionStack.Peek();
                            newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.SimpleAssignmentExpression:
                case SyntaxKind.LeftShiftAssignmentExpression:
                case SyntaxKind.RightShiftAssignmentExpression:
                case SyntaxKind.OrAssignmentExpression:
                case SyntaxKind.AndAssignmentExpression:
                case SyntaxKind.ExclusiveOrAssignmentExpression:
                    {
                        var assignment = (AssignmentExpressionSyntax)node.Instruction;
                        var symbol = SemanticModel.GetSymbolInfo(assignment.Left).Symbol;

                        if (ExplodedGraphWalker.IsSymbolTracked(symbol))
                        {
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.DefaultExpression:
                    {
                        var typeSymbol = SemanticModel.GetTypeInfo(node.Instruction).Type;
                        var sv = newProgramState.ExpressionStack.Peek();

                        var isReferenceOrNullable = typeSymbol.IsReferenceType ||
                            typeSymbol.OriginalDefinition.Is(KnownType.System_Nullable_T);

                        newProgramState = isReferenceOrNullable
                            ? sv.SetConstraint(ObjectConstraint.Null, programState)
                            : SetNonNullConstraintIfValueType(typeSymbol, sv, programState);
                        break;
                    }

                case SyntaxKind.AsExpression:
                    {
                        SymbolicValue argSV;
                        node.ProgramState.PopValue(out argSV);
                        if (argSV.HasConstraint(ObjectConstraint.Null, newProgramState))
                        {
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = sv.SetConstraint(ObjectConstraint.Null, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.SimpleMemberAccessExpression:
                case SyntaxKind.PointerMemberAccessExpression:
                    {
                        var typeSymbol = SemanticModel.GetTypeInfo(node.Instruction).Type;
                        var sv = newProgramState.ExpressionStack.Peek();
                        newProgramState = SetNonNullConstraintIfValueType(typeSymbol, sv, newProgramState);
                        break;
                    }

                case SyntaxKind.ObjectCreationExpression:
                case SyntaxKind.AnonymousObjectCreationExpression:
                case SyntaxKind.StackAllocArrayCreationExpression:
                    {
                        var sv = newProgramState.ExpressionStack.Peek();
                        newProgramState = sv.SetConstraint(ObjectConstraint.NotNull, newProgramState);
                        break;
                    }
            }

            return newProgramState;
        }

        public override ProgramState PostProcessBlock(Block block, ProgramState programState)
        {
            var newProgramState = programState;

            var binaryBranchBlock = block as BinaryBranchBlock;
            if (binaryBranchBlock != null &&
                binaryBranchBlock.BranchingNode.IsKind(SyntaxKind.ForEachStatement))
            {
                var foreachVariableSymbol = SemanticModel.GetDeclaredSymbol(binaryBranchBlock.BranchingNode);
                var sv = newProgramState.ExpressionStack.Peek();
                newProgramState = SetNonNullConstraintIfValueType(foreachVariableSymbol, sv, newProgramState);
            }

            return newProgramState;
        }

        private static ProgramState SetNonNullConstraintIfValueType(ITypeSymbol typeSymbol,
            SymbolicValue symbolicValue, ProgramState programState)
        {
            if (symbolicValue.HasConstraint(ObjectConstraint.NotNull, programState))
            {
                return programState;
            }

            var isDefinitelyNotNull = symbolicValue != SymbolicValue.Null &&
                typeSymbol != null &&
                typeSymbol.IsValueType &&
                typeSymbol.TypeKind != TypeKind.Pointer;

            return isDefinitelyNotNull
                ? symbolicValue.SetConstraint(ObjectConstraint.NotNull, programState)
                : programState;
        }

        private static ProgramState SetNonNullConstraintIfValueType(ISymbol symbol, SymbolicValue symbolicValue,
            ProgramState programState)
        {
            return SetNonNullConstraintIfValueType(symbol.GetSymbolType(), symbolicValue, programState);
        }
    }
}

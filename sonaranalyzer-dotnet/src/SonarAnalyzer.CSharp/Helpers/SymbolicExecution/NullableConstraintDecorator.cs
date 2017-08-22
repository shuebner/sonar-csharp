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
    internal class NullableConstraintDecorator : ConstraintDecorator
    {
        private const string HasValueLiteral = "HasValue";
        private const string ValueLiteral = "Value";

        public NullableConstraintDecorator(CSharpExplodedGraphWalker explodedGraphWalker)
            : base(explodedGraphWalker)
        {
        }

        public override ProgramState PostProcessInstruction(ExplodedGraphNode node, ProgramState programState)
        {
            var newProgramState = programState;

            switch (node.Instruction.Kind())
            {
                case SyntaxKind.NullLiteralExpression:
                    break; // Constant literal expressions are already handled by exploded graph walker

                case SyntaxKind.SimpleMemberAccessExpression:
                    {
                        var memberAccess = (MemberAccessExpressionSyntax)node.Instruction;
                        var symbol = SemanticModel.GetSymbolInfo(memberAccess.Expression).Symbol;

                        if (symbol.IsNullable() &&
                            ExplodedGraphWalker.IsSymbolTracked(symbol))
                        {
                            var sv = newProgramState.GetSymbolValue(symbol);
                            if (memberAccess.Name.Identifier.ValueText == ValueLiteral)
                            {
                                newProgramState = SetConstraint(sv, NullableValueConstraint.HasValue,
                                    memberAccess.Expression, newProgramState);
                            }
                        }
                        break;
                    }

                case SyntaxKind.ObjectCreationExpression:
                    {
                        var sv = newProgramState.ExpressionStack.Peek();
                        var methodSymbol = SemanticModel.GetSymbolInfo(node.Instruction).Symbol as IMethodSymbol;

                        if (methodSymbol != null &&
                            IsNullableCtorCall(methodSymbol))
                        {
                            var constraintToApply = methodSymbol.Parameters.Length == 0
                                ? NullableValueConstraint.NoValue
                                : NullableValueConstraint.HasValue;
                            newProgramState = SetConstraint(sv, constraintToApply, node.Instruction, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.SimpleAssignmentExpression:
                    {
                        var symbol = SemanticModel.GetSymbolInfo(node.Instruction).Symbol;
                        newProgramState = SetConstraintIfTracked(node.Instruction, symbol, newProgramState);
                        break;
                    }

                case SyntaxKind.VariableDeclarator:
                    {
                        var variableDeclarator = (VariableDeclaratorSyntax)node.Instruction;
                        var symbol = SemanticModel.GetDeclaredSymbol(node.Instruction);

                        if (variableDeclarator.Initializer?.Value != null)
                        {
                            newProgramState = SetConstraintIfTracked(variableDeclarator, symbol, newProgramState);
                        }
                        break;
                    }
            }

            return newProgramState;
        }

        private ProgramState SetConstraintIfTracked(SyntaxNode node, ISymbol symbol, ProgramState programState)
        {
            if (!ExplodedGraphWalker.IsSymbolTracked(symbol) ||
                !symbol.IsNullable())
            {
                return programState;
            }

            var sv = programState.GetSymbolValue(symbol) as NullableSymbolicValue;
            if (sv?.WrappedSymbolicValue == null)
            {
                return programState;
            }

            if (sv.WrappedSymbolicValue.HasConstraint(ObjectConstraint.Null, programState))
            {
                return SetConstraint(sv, NullableValueConstraint.NoValue, node, programState);
            }
            else if (sv.WrappedSymbolicValue.HasConstraint(ObjectConstraint.NotNull, programState))
            {
                return SetConstraint(sv, NullableValueConstraint.HasValue, node, programState);
            }

            return programState;
        }

        private bool IsValuePropertyAccess(MemberAccessExpressionSyntax memberAccess)
        {
            return memberAccess.Name.Identifier.ValueText == ValueLiteral &&
                memberAccess.Expression.IsNullable(SemanticModel);
        }

        private static bool IsNullableCtorCall(IMethodSymbol nullableConstructorCall)
        {
            return nullableConstructorCall != null &&
                nullableConstructorCall.MethodKind == MethodKind.Constructor &&
                nullableConstructorCall.ReceiverType.OriginalDefinition.Is(KnownType.System_Nullable_T);
        }
    }
}
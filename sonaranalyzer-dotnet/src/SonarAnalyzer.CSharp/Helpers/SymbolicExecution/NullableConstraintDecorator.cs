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

                        if (IsValuePropertyAccess(memberAccess))
                        {
                            var typeSymbol = SemanticModel.GetTypeInfo(memberAccess).Type;
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = sv.SetConstraint(NullableValueConstraint.HasValue, newProgramState);
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
                            newProgramState = methodSymbol.Parameters.Length == 0
                                ? sv.SetConstraint(NullableValueConstraint.NoValue, newProgramState)
                                : sv.SetConstraint(NullableValueConstraint.HasValue, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.VariableDeclarator:
                    {
                        var varDeclarator = (VariableDeclaratorSyntax)node.Instruction;
                        var symbol = SemanticModel.GetDeclaredSymbol(node.Instruction);

                        if (varDeclarator.Initializer?.Value != null &&
                            ExplodedGraphWalker.IsSymbolTracked(symbol) &&
                            symbol.GetSymbolType().OriginalDefinition.Is(KnownType.System_Nullable_T))
                        {
                            var sv = node.ProgramState.ExpressionStack.Peek();

                            if (sv.HasConstraint(ObjectConstraint.NotNull, newProgramState))
                            {
                                newProgramState = sv.SetConstraint(NullableValueConstraint.HasValue, newProgramState);
                            }

                            if (sv.HasConstraint(ObjectConstraint.Null, newProgramState))
                            {
                                newProgramState = sv.SetConstraint(NullableValueConstraint.NoValue, newProgramState);
                            }
                        }
                        break;
                    }
            }

            return newProgramState;
        }

        private bool IsValuePropertyAccess(MemberAccessExpressionSyntax memberAccess)
        {
            return memberAccess.Name.Identifier.ValueText == ValueLiteral &&
                (SemanticModel.GetTypeInfo(memberAccess.Expression).Type?.OriginalDefinition)
                    .Is(KnownType.System_Nullable_T);
        }

        private static bool IsNullableCtorCall(IMethodSymbol nullableConstructorCall)
        {
            return nullableConstructorCall != null &&
                nullableConstructorCall.MethodKind == MethodKind.Constructor &&
                nullableConstructorCall.ReceiverType.OriginalDefinition.Is(KnownType.System_Nullable_T);
        }
    }
}
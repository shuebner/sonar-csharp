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

using System.Linq;
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

        public override ProgramState PostProcessInstruction(SyntaxNode instruction, ProgramState preProgramState,
            ProgramState postProgramState)
        {
            var newProgramState = postProgramState;

            switch (instruction.Kind())
            {
                case SyntaxKind.NullLiteralExpression:
                    break; // Constant literal expressions are already handled by exploded graph walker

                case SyntaxKind.IdentifierName:
                    {
                        var symbol = base.semanticModel.GetSymbolInfo(instruction).Symbol;

                        if (base.explodedGraphWalker.IsSymbolTracked(symbol))
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
                        var operand = instruction is PostfixUnaryExpressionSyntax
                            ? ((PostfixUnaryExpressionSyntax)instruction).Operand
                            : ((PrefixUnaryExpressionSyntax)instruction).Operand;
                        var symbol = base.semanticModel.GetSymbolInfo(operand).Symbol;

                        if (base.explodedGraphWalker.IsSymbolTracked(symbol))
                        {
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.VariableDeclarator:
                    {
                        var symbol = base.semanticModel.GetDeclaredSymbol(instruction);

                        if (base.explodedGraphWalker.IsSymbolTracked(symbol) &&
                            !preProgramState.ExpressionStack.IsEmpty)
                        {
                            var sv = preProgramState.ExpressionStack.Peek();
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
                        var symbol = base.semanticModel.GetSymbolInfo(((AssignmentExpressionSyntax)instruction).Left).Symbol;

                        if (base.explodedGraphWalker.IsSymbolTracked(symbol))
                        {
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        }
                        break;
                    }

                case SyntaxKind.DefaultExpression:
                    {
                        var typeSymbol = base.semanticModel.GetTypeInfo(instruction).Type;
                        var sv = newProgramState.ExpressionStack.Peek();

                        var isReferenceOrNullable = typeSymbol.IsReferenceType ||
                            typeSymbol.OriginalDefinition.Is(KnownType.System_Nullable_T);

                        newProgramState = isReferenceOrNullable
                            ? sv.SetConstraint(ObjectConstraint.Null, postProgramState)
                            : SetNonNullConstraintIfValueType(typeSymbol, sv, postProgramState);
                        break;
                    }

                case SyntaxKind.AsExpression:
                    {
                        SymbolicValue argSV;
                        preProgramState.PopValue(out argSV);
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
                        var typeSymbol = base.semanticModel.GetTypeInfo(instruction).Type;
                        var sv = newProgramState.ExpressionStack.Peek();
                        newProgramState = SetNonNullConstraintIfValueType(typeSymbol, sv, newProgramState);
                        break;
                    }

                case SyntaxKind.ObjectCreationExpression:
                    {
                        var sv = newProgramState.ExpressionStack.Peek();
                        var symbol = this.semanticModel.GetSymbolInfo(instruction).Symbol as IMethodSymbol;
                        if (symbol != null)
                        {
                            newProgramState = IsEmptyNullableCtorCall(symbol)
                                ? sv.SetConstraint(ObjectConstraint.Null, newProgramState)
                                : sv.SetConstraint(ObjectConstraint.NotNull, newProgramState);
                        }
                        break;
                    }

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
                var foreachVariableSymbol = this.semanticModel.GetDeclaredSymbol(binaryBranchBlock.BranchingNode);
                var sv = newProgramState.ExpressionStack.Peek();
                newProgramState = SetNonNullConstraintIfValueType(foreachVariableSymbol, sv, newProgramState);
            }

            return newProgramState;
        }

        private static ProgramState SetNonNullConstraintIfValueType(ITypeSymbol typeSymbol,
            SymbolicValue symbolicValue, ProgramState programState)
        {
            var isDefinitelyNotNull = !symbolicValue.HasConstraint(ObjectConstraint.NotNull, programState) &&
                IsNonNullableValueType(typeSymbol) &&
                !IsValueTypeWithOverloadedNullCompatibleOpEquals(typeSymbol) &&
                !IsPointer(typeSymbol);

            return isDefinitelyNotNull
                ? symbolicValue.SetConstraint(ObjectConstraint.NotNull, programState)
                : programState;
        }

        private static ProgramState SetNonNullConstraintIfValueType(ISymbol symbol, SymbolicValue symbolicValue,
            ProgramState programState)
        {
            return SetNonNullConstraintIfValueType(symbol.GetSymbolType(), symbolicValue, programState);
        }

        private static bool IsPointer(ITypeSymbol typeSymbol)
        {
            return typeSymbol?.TypeKind == TypeKind.Pointer;
        }

        private static bool IsValueTypeWithOverloadedNullCompatibleOpEquals(ITypeSymbol type)
        {
            return type != null &&
                type.IsValueType &&
                type.GetMembers("op_Equality")
                    .OfType<IMethodSymbol>()
                    .Any(m => m.Parameters.Any(p => IsNullCompatibleType(p.Type)));
        }

        private static bool IsNullCompatibleType(ITypeSymbol type)
        {
            if (type == null)
            {
                return false;
            }

            return !type.IsValueType ||
                type.OriginalDefinition.Is(KnownType.System_Nullable_T);
        }

        private static bool IsNonNullableValueType(ITypeSymbol type)
        {
            return type != null &&
                type.IsValueType &&
                !type.OriginalDefinition.Is(KnownType.System_Nullable_T);
        }

        private static bool IsEmptyNullableCtorCall(IMethodSymbol nullableConstructorCall)
        {
            return nullableConstructorCall != null &&
                nullableConstructorCall.MethodKind == MethodKind.Constructor &&
                nullableConstructorCall.ReceiverType.OriginalDefinition.Is(KnownType.System_Nullable_T) &&
                nullableConstructorCall.Parameters.Length == 0;
        }
    }
}

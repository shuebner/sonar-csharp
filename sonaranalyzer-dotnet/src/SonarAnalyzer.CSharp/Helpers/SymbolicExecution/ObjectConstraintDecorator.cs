using System;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis.CSharp
{
    public class ObjectConstraintDecorator : IDomainConstraintDecorator
    {
        private readonly SemanticModel semanticModel;

        public ObjectConstraintDecorator(SemanticModel semanticModel)
        {
            this.semanticModel = semanticModel;
        }

        public ProgramState PreProcessInstruction(SyntaxNode instruction, ProgramState programState)
        {
            throw new NotImplementedException();
        }

        public ProgramState PostProcessInstruction(SyntaxNode instruction, ProgramState preProgramState,
            ProgramState postProgramState)
        {
            var newProgramState = postProgramState;

            switch (instruction.Kind())
            {
                case SyntaxKind.NullLiteralExpression:
                    break; // Constant literal expressions are already handled by exploded graph walker

                case SyntaxKind.IdentifierName:
                    {
                        var symbol = this.semanticModel.GetSymbolInfo(instruction).Symbol;
                        // TODO: Only if IsSymbolTracked
                        var sv = newProgramState.ExpressionStack.Peek();
                        newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
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
                        var symbol = this.semanticModel.GetSymbolInfo(operand).Symbol;
                        // TODO: Only if IsSymbolTracked
                        var sv = newProgramState.ExpressionStack.Peek();
                        newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        break;
                    }

                case SyntaxKind.VariableDeclarator:
                    {
                        var symbol = this.semanticModel.GetDeclaredSymbol(instruction);
                        // TODO: Only if IsSymbolTracked
                        var sv = newProgramState.ExpressionStack.Peek();
                        newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        break;
                    }

                case SyntaxKind.SimpleAssignmentExpression:
                case SyntaxKind.LeftShiftAssignmentExpression:
                case SyntaxKind.RightShiftAssignmentExpression:
                case SyntaxKind.OrAssignmentExpression:
                case SyntaxKind.AndAssignmentExpression:
                case SyntaxKind.ExclusiveOrAssignmentExpression:
                    {
                        var symbol = this.semanticModel.GetSymbolInfo(((AssignmentExpressionSyntax)instruction).Left).Symbol;
                        // TODO: Only if IsSymbolTracked
                        var sv = newProgramState.ExpressionStack.Peek();
                        newProgramState = SetNonNullConstraintIfValueType(symbol, sv, newProgramState);
                        break;
                    }

                case SyntaxKind.DefaultExpression:
                    {
                        var typeSymbol = this.semanticModel.GetTypeInfo(instruction).Type;
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
                        var sv = newProgramState.ExpressionStack.Peek();
                        newProgramState = SetNonNullConstraintIfValueType(instruction, sv, newProgramState);
                        break;
                    }
            }

            return newProgramState;
        }

        private static ProgramState SetNonNullConstraintIfValueType(ITypeSymbol typeSymbol, SymbolicValue symbolicValue, ProgramState programState)
        {
            var isDefinitelyNotNull = !symbolicValue.HasConstraint(ObjectConstraint.NotNull, programState) &&
                IsNonNullableValueType(typeSymbol) &&
                !IsValueTypeWithOverloadedNullCompatibleOpEquals(typeSymbol) &&
                !IsPointer(typeSymbol);

            return isDefinitelyNotNull
                ? symbolicValue.SetConstraint(ObjectConstraint.NotNull, programState)
                : programState;
        }

        private static ProgramState SetNonNullConstraintIfValueType(ISymbol symbol, SymbolicValue symbolicValue, ProgramState programState)
        {
            return SetNonNullConstraintIfValueType(symbol.GetSymbolType(), symbolicValue, programState);
        }

        private ProgramState SetNonNullConstraintIfValueType(SyntaxNode node, SymbolicValue symbolicValue, ProgramState programState)
        {
            return SetNonNullConstraintIfValueType(this.semanticModel.GetTypeInfo(node).Type, symbolicValue, programState);
        }

        private static bool IsPointer(ITypeSymbol typeSymbol)
        {
            return typeSymbol?.TypeKind == TypeKind.Pointer;
        }

        private static bool IsValueTypeWithOverloadedNullCompatibleOpEquals(ITypeSymbol type)
        {
            if (type == null ||
                !type.IsValueType)
            {
                return false;
            }

            return type.GetMembers("op_Equality")
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
    }
}

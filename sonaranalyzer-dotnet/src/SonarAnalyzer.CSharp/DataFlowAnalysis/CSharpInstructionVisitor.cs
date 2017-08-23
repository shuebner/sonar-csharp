using System;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SonarAnalyzer.DataFlowAnalysis.CSharp;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.DataFlowAnalysis
{
    internal class CSharpInstructionVisitor
    {
        public AbstractExplodedGraphWalker ExplodedGraphWalker { get; }

        public CSharpInstructionVisitor(AbstractExplodedGraphWalker explodedGraphWalker)
        {
            ExplodedGraphWalker = explodedGraphWalker;
        }

        private ISymbol GetSymbol(SyntaxNode instruction) =>
            ExplodedGraphWalker.SemanticModel.GetSymbolInfo(instruction).Symbol;

        private ISymbol GetType(SyntaxNode instruction) =>
            ExplodedGraphWalker.SemanticModel.GetTypeInfo(instruction).Type;

        private ISymbol GetDeclaredSymbol(VariableDeclaratorSyntax declarator) =>
            ExplodedGraphWalker.SemanticModel.GetDeclaredSymbol(declarator);

        public ProgramState VisitInstruction(SyntaxNode instruction, ProgramState programState)
        {
            var expression = instruction as ExpressionSyntax;
            var parenthesizedExpression = expression?.GetSelfOrTopParenthesizedExpression();

            var newProgramState = programState;

            switch (instruction.Kind())
            {
                case SyntaxKind.CastExpression:
                case SyntaxKind.CheckedExpression:
                case SyntaxKind.UncheckedExpression:
                    // Do nothing
                    break;

                case SyntaxKind.VariableDeclarator:
                    newProgramState = VisitVariableDeclarator((VariableDeclaratorSyntax)instruction, newProgramState);
                    break;
                case SyntaxKind.SimpleAssignmentExpression:
                    newProgramState = VisitSimpleAssignment((AssignmentExpressionSyntax)instruction, newProgramState);
                    break;

                case SyntaxKind.OrAssignmentExpression:
                    newProgramState = VisitBooleanBinaryOpAssignment(newProgramState,
                        (AssignmentExpressionSyntax)instruction, (l, r) => new OrSymbolicValue(l, r));
                    break;
                case SyntaxKind.AndAssignmentExpression:
                    newProgramState = VisitBooleanBinaryOpAssignment(newProgramState,
                        (AssignmentExpressionSyntax)instruction, (l, r) => new AndSymbolicValue(l, r));
                    break;
                case SyntaxKind.ExclusiveOrAssignmentExpression:
                    newProgramState = VisitBooleanBinaryOpAssignment(newProgramState,
                        (AssignmentExpressionSyntax)instruction, (l, r) => new XorSymbolicValue(l, r));
                    break;

                case SyntaxKind.SubtractAssignmentExpression:
                case SyntaxKind.AddAssignmentExpression:
                case SyntaxKind.DivideAssignmentExpression:
                case SyntaxKind.MultiplyAssignmentExpression:
                case SyntaxKind.ModuloAssignmentExpression:
                case SyntaxKind.LeftShiftAssignmentExpression:
                case SyntaxKind.RightShiftAssignmentExpression:
                    newProgramState = VisitOpAssignment((AssignmentExpressionSyntax)instruction, newProgramState);
                    break;

                case SyntaxKind.PreIncrementExpression:
                case SyntaxKind.PreDecrementExpression:
                    newProgramState = VisitPrefixIncrement((PrefixUnaryExpressionSyntax)instruction, newProgramState);
                    break;

                case SyntaxKind.PostIncrementExpression:
                case SyntaxKind.PostDecrementExpression:
                    newProgramState = VisitPostfixIncrement((PostfixUnaryExpressionSyntax)instruction, newProgramState);
                    break;

                case SyntaxKind.IdentifierName:
                    newProgramState = VisitIdentifier((IdentifierNameSyntax)instruction, newProgramState);
                    break;

                case SyntaxKind.BitwiseOrExpression:
                    newProgramState = VisitBinaryOperator(newProgramState, (l, r) => new OrSymbolicValue(l, r));
                    break;
                case SyntaxKind.BitwiseAndExpression:
                    newProgramState = VisitBinaryOperator(newProgramState, (l, r) => new AndSymbolicValue(l, r));
                    break;
                case SyntaxKind.ExclusiveOrExpression:
                    newProgramState = VisitBinaryOperator(newProgramState, (l, r) => new XorSymbolicValue(l, r));
                    break;

                case SyntaxKind.LessThanExpression:
                    newProgramState = VisitComparisonBinaryOperator(newProgramState,
                        (BinaryExpressionSyntax)instruction,
                        (l, r) => new ComparisonSymbolicValue(ComparisonKind.Less, l, r));
                    break;
                case SyntaxKind.LessThanOrEqualExpression:
                    newProgramState = VisitComparisonBinaryOperator(newProgramState,
                        (BinaryExpressionSyntax)instruction,
                        (l, r) => new ComparisonSymbolicValue(ComparisonKind.LessOrEqual, l, r));
                    break;
                case SyntaxKind.GreaterThanExpression:
                    newProgramState = VisitComparisonBinaryOperator(newProgramState,
                        (BinaryExpressionSyntax)instruction,
                        (l, r) => new ComparisonSymbolicValue(ComparisonKind.Less, r, l));
                    break;
                case SyntaxKind.GreaterThanOrEqualExpression:
                    newProgramState = VisitComparisonBinaryOperator(newProgramState,
                        (BinaryExpressionSyntax)instruction,
                        (l, r) => new ComparisonSymbolicValue(ComparisonKind.LessOrEqual, r, l));
                    break;

                case SyntaxKind.SubtractExpression:
                case SyntaxKind.AddExpression:
                case SyntaxKind.DivideExpression:
                case SyntaxKind.MultiplyExpression:
                case SyntaxKind.ModuloExpression:
                case SyntaxKind.LeftShiftExpression:
                case SyntaxKind.RightShiftExpression:
                    newProgramState = newProgramState
                        .PopValues(2)
                        .PushValue(new SymbolicValue());
                    break;

                case SyntaxKind.EqualsExpression:
                    var binary = (BinaryExpressionSyntax)instruction;
                    newProgramState = IsOperatorOnObject(instruction)
                        ? VisitReferenceEquals(binary, newProgramState)
                        : VisitValueEquals(newProgramState);

                    break;

                case SyntaxKind.NotEqualsExpression:
                    newProgramState = IsOperatorOnObject(instruction)
                        ? VisitBinaryOperator(newProgramState, (l, r) => new ReferenceNotEqualsSymbolicValue(l, r))
                        : VisitBinaryOperator(newProgramState, (l, r) => new ValueNotEqualsSymbolicValue(l, r));
                    break;

                case SyntaxKind.BitwiseNotExpression:
                case SyntaxKind.UnaryMinusExpression:
                case SyntaxKind.UnaryPlusExpression:
                case SyntaxKind.AddressOfExpression:
                case SyntaxKind.PointerIndirectionExpression:
                case SyntaxKind.MakeRefExpression:
                case SyntaxKind.RefTypeExpression:
                case SyntaxKind.RefValueExpression:
                case SyntaxKind.MemberBindingExpression:
                case SyntaxKind.AwaitExpression:
                case SyntaxKind.AsExpression:
                case SyntaxKind.IsExpression:
                    newProgramState = newProgramState
                        .PopValue()
                        .PushValue(new SymbolicValue());
                    break;

                case SyntaxKind.SimpleMemberAccessExpression:
                case SyntaxKind.PointerMemberAccessExpression:
                    newProgramState = VisitMemberAccess((MemberAccessExpressionSyntax)instruction, newProgramState);
                    break;


                case SyntaxKind.LogicalNotExpression:
                    {
                        SymbolicValue sv;
                        newProgramState = newProgramState
                            .PopValue(out sv)
                            .PushValue(new LogicalNotSymbolicValue(sv));
                    }
                    break;

                case SyntaxKind.TrueLiteralExpression:
                    newProgramState = newProgramState.PushValue(SymbolicValue.True);
                    break;
                case SyntaxKind.FalseLiteralExpression:
                    newProgramState = newProgramState.PushValue(SymbolicValue.False);
                    break;
                case SyntaxKind.NullLiteralExpression:
                    newProgramState = newProgramState.PushValue(SymbolicValue.Null);
                    break;

                case SyntaxKind.ThisExpression:
                    newProgramState = newProgramState.PushValue(SymbolicValue.This);
                    break;
                case SyntaxKind.BaseExpression:
                    newProgramState = newProgramState.PushValue(SymbolicValue.Base);
                    break;

                case SyntaxKind.GenericName:
                case SyntaxKind.AliasQualifiedName:
                case SyntaxKind.QualifiedName:
                case SyntaxKind.PredefinedType:
                case SyntaxKind.NullableType:
                case SyntaxKind.OmittedArraySizeExpression:
                case SyntaxKind.AnonymousMethodExpression:
                case SyntaxKind.ParenthesizedLambdaExpression:
                case SyntaxKind.SimpleLambdaExpression:
                case SyntaxKind.QueryExpression:
                case SyntaxKind.ArgListExpression:
                case SyntaxKind.CharacterLiteralExpression:
                case SyntaxKind.StringLiteralExpression:
                case SyntaxKind.NumericLiteralExpression:
                case SyntaxKind.SizeOfExpression:
                case SyntaxKind.TypeOfExpression:
                case SyntaxKind.ArrayCreationExpression:
                case SyntaxKind.ImplicitArrayCreationExpression:
                case SyntaxKind.StackAllocArrayCreationExpression:
                case SyntaxKind.DefaultExpression:
                    newProgramState = newProgramState.PushValue(new SymbolicValue());
                    break;

                case SyntaxKind.AnonymousObjectCreationExpression:
                    newProgramState = newProgramState
                        .PopValues(((AnonymousObjectCreationExpressionSyntax)instruction).Initializers.Count)
                        .PushValue(new SymbolicValue());
                    break;

                case SyntaxKind.InterpolatedStringExpression:
                    newProgramState = newProgramState
                        .PopValues(((InterpolatedStringExpressionSyntax)instruction).Contents.OfType<InterpolationSyntax>().Count())
                        .PushValue(new SymbolicValue());
                    break;

                case SyntaxKind.ObjectCreationExpression:
                    newProgramState = VisitObjectCreation((ObjectCreationExpressionSyntax)instruction, newProgramState);
                    break;

                case SyntaxKind.ElementAccessExpression:
                    newProgramState = newProgramState
                        .PopValues((((ElementAccessExpressionSyntax)instruction).ArgumentList?.Arguments.Count ?? 0) + 1)
                        .PushValue(new SymbolicValue());
                    break;

                case SyntaxKind.ImplicitElementAccess:
                    newProgramState = newProgramState
                        .PopValues(((ImplicitElementAccessSyntax)instruction).ArgumentList?.Arguments.Count ?? 0)
                        .PushValue(new SymbolicValue());
                    break;

                case SyntaxKind.ObjectInitializerExpression:
                case SyntaxKind.ArrayInitializerExpression:
                case SyntaxKind.CollectionInitializerExpression:
                case SyntaxKind.ComplexElementInitializerExpression:
                    newProgramState = VisitInitializer(instruction, parenthesizedExpression, newProgramState);
                    break;

                case SyntaxKind.ArrayType:
                    newProgramState = newProgramState
                        .PopValues(((ArrayTypeSyntax)instruction).RankSpecifiers.SelectMany(rs => rs.Sizes).Count());
                    break;

                case SyntaxKind.ElementBindingExpression:
                    newProgramState = newProgramState
                        .PopValues(((ElementBindingExpressionSyntax)instruction).ArgumentList?.Arguments.Count ?? 0)
                        .PushValue(new SymbolicValue());
                    break;

                case SyntaxKind.InvocationExpression:
                    {
                        var invocation = (InvocationExpressionSyntax)instruction;
                        var invocationVisitor = new InvocationVisitor(invocation, ExplodedGraphWalker.SemanticModel, newProgramState);
                        newProgramState = invocationVisitor.ProcessInvocation();

                        if (invocation.Expression.IsOnThis() && !invocation.IsNameof(ExplodedGraphWalker.SemanticModel))
                        {
                            newProgramState = newProgramState.RemoveSymbols(ExplodedGraphWalker.IsFieldSymbol);
                        }
                    }
                    break;

                default:
                    throw new NotImplementedException($"{instruction.Kind()}");
            }

            return newProgramState;
        }

        #region VisitExpression

        private ProgramState VisitMemberAccess(MemberAccessExpressionSyntax memberAccess, ProgramState programState)
        {
            SymbolicValue memberExpression;
            var newProgramState = programState.PopValue(out memberExpression);

            SymbolicValue sv = null;
            var identifier = memberAccess.Name as IdentifierNameSyntax;
            if (identifier != null)
            {
                var fieldSymbol = GetSymbol(identifier) as IFieldSymbol;
                if (fieldSymbol != null &&
                    (memberAccess.IsOnThis() || fieldSymbol.IsConst))
                {
                    sv = newProgramState.GetSymbolValue(fieldSymbol);
                    if (sv == null)
                    {
                        newProgramState = CreateAndStoreFieldSymbolicValue(newProgramState, fieldSymbol, out sv);
                    }
                }
            }

            if (sv == null)
            {
                sv = new MemberAccessSymbolicValue(memberExpression, memberAccess.Name.Identifier.ValueText);
            }

            return newProgramState.PushValue(sv);
        }

        public static ProgramState CreateAndStoreFieldSymbolicValue(ProgramState programState, IFieldSymbol fieldSymbol,
            out SymbolicValue symbolicValue)
        {
            if (!fieldSymbol.IsConst ||
                !fieldSymbol.HasConstantValue)
            {
                // TODO: handle readonly initialized inline with null
                symbolicValue = new SymbolicValue();
            }
            else
            {
                var boolValue = fieldSymbol.ConstantValue as bool?;
                if (boolValue.HasValue)
                {
                    symbolicValue = boolValue.Value
                        ? SymbolicValue.True
                        : SymbolicValue.False;
                }
                else
                {
                    symbolicValue = fieldSymbol.ConstantValue == null
                        ? SymbolicValue.Null
                        : new SymbolicValue();
                }
            }

            return programState.StoreSymbolicValue(fieldSymbol, symbolicValue);
        }

        private bool IsOperatorOnObject(SyntaxNode instruction)
        {
            var operatorSymbol = GetSymbol(instruction) as IMethodSymbol;
            return operatorSymbol != null &&
                operatorSymbol.ContainingType.Is(KnownType.System_Object);
        }

        private static ProgramState VisitValueEquals(ProgramState programState)
        {
            SymbolicValue leftSymbol;
            SymbolicValue rightSymbol;

            var newProgramState = programState
                .PopValue(out rightSymbol)
                .PopValue(out leftSymbol);

            var equals = new ValueEqualsSymbolicValue(leftSymbol, rightSymbol);
            newProgramState = newProgramState.PushValue(equals);
            return InvocationVisitor.SetConstraintOnValueEquals(equals, newProgramState);
        }

        private ProgramState VisitReferenceEquals(BinaryExpressionSyntax equals, ProgramState programState)
        {
            SymbolicValue leftSymbol;
            SymbolicValue rightSymbol;

            var newProgramState = programState
                .PopValue(out rightSymbol)
                .PopValue(out leftSymbol);

            return new InvocationVisitor.ReferenceEqualsConstraintHandler(leftSymbol, rightSymbol,
                equals.Left, equals.Right, newProgramState, ExplodedGraphWalker.SemanticModel).PushWithConstraint();
        }

        private ProgramState VisitComparisonBinaryOperator(ProgramState programState, BinaryExpressionSyntax comparison,
            Func<SymbolicValue, SymbolicValue, SymbolicValue> svFactory)
        {
            SymbolicValue leftSymbol;
            SymbolicValue rightSymbol;

            var newProgramState = programState
                .PopValue(out rightSymbol)
                .PopValue(out leftSymbol);

            var op = GetSymbol(comparison) as IMethodSymbol;

            var isValueTypeOperator = op?.ContainingType?.IsValueType ?? false;

            var isLiftedOperator = isValueTypeOperator &&
                (programState.HasConstraint(leftSymbol, ObjectConstraint.Null)
                || programState.HasConstraint(rightSymbol, ObjectConstraint.Null));

            var comparisonValue = isLiftedOperator ? SymbolicValue.False : svFactory(leftSymbol, rightSymbol);

            return newProgramState.PushValue(comparisonValue);
        }

        private static ProgramState VisitBinaryOperator(ProgramState programState,
            Func<SymbolicValue, SymbolicValue, SymbolicValue> svFactory)
        {
            SymbolicValue leftSymbol;
            SymbolicValue rightSymbol;

            return programState
                .PopValue(out rightSymbol)
                .PopValue(out leftSymbol)
                .PushValue(svFactory(leftSymbol, rightSymbol));
        }

        private ProgramState VisitBooleanBinaryOpAssignment(ProgramState programState,
            AssignmentExpressionSyntax assignment,
            Func<SymbolicValue, SymbolicValue, SymbolicValue> symbolicValueFactory)
        {
            var symbol = GetSymbol(assignment.Left);

            SymbolicValue leftSymbol;
            SymbolicValue rightSymbol;

            var newProgramState = programState
                .PopValue(out rightSymbol)
                .PopValue(out leftSymbol);

            var sv = symbolicValueFactory(leftSymbol, rightSymbol);
            newProgramState = newProgramState.PushValue(sv);

            return StoreSymbolicValueIfSymbolIsTracked(symbol, sv, newProgramState);
        }

        private ProgramState VisitObjectCreation(ObjectCreationExpressionSyntax ctor, ProgramState programState)
        {
            var methodSymbol = GetSymbol(ctor) as IMethodSymbol;

            SymbolicValue sv;
            var numberOfArgsToPop = ctor.ArgumentList?.Arguments.Count ?? 0;
            var newProgramState = programState;

            if (methodSymbol?.ReceiverType?.OriginalDefinition.Is(KnownType.System_Nullable_T) == true)
            {
                if (methodSymbol.Parameters.Length == 0)
                {
                    sv = new NullableSymbolicValue(null);
                }
                else
                {
                    SymbolicValue innverSv;
                    newProgramState = newProgramState.PopValue(out innverSv);
                    numberOfArgsToPop--;
                    sv = new NullableSymbolicValue(innverSv);
                }
            }
            else
            {
                sv = new SymbolicValue();
            }

            return newProgramState
                .PopValues(numberOfArgsToPop)
                .PushValue(sv);
        }

        private static ProgramState VisitInitializer(SyntaxNode instruction, ExpressionSyntax parenthesizedExpression,
            ProgramState programState)
        {
            var initializer = (InitializerExpressionSyntax)instruction;
            var newProgramState = programState.PopValues(initializer.Expressions.Count);

            if (!(parenthesizedExpression.Parent is ObjectCreationExpressionSyntax) &&
                !(parenthesizedExpression.Parent is ArrayCreationExpressionSyntax) &&
                !(parenthesizedExpression.Parent is AnonymousObjectCreationExpressionSyntax) &&
                !(parenthesizedExpression.Parent is ImplicitArrayCreationExpressionSyntax))
            {
                newProgramState = newProgramState.PushValue(new SymbolicValue());
            }

            return newProgramState;
        }

        private ProgramState VisitIdentifier(IdentifierNameSyntax identifier, ProgramState programState)
        {
            var newProgramState = programState;
            var symbol = GetSymbol(identifier);
            var typeSymbol = GetType(identifier);
            var sv = newProgramState.GetSymbolValue(symbol);

            if (sv == null)
            {
                var fieldSymbol = symbol as IFieldSymbol;
                if (fieldSymbol != null) // TODO: Fix me when implementing SLVS-1130
                {
                    newProgramState = CreateAndStoreFieldSymbolicValue(newProgramState, fieldSymbol, out sv);
                }
                else
                {
                    sv = new SymbolicValue();
                }
            }
            newProgramState = newProgramState.PushValue(sv);

            var parenthesized = identifier.GetSelfOrTopParenthesizedExpression();
            var argument = parenthesized.Parent as ArgumentSyntax;
            if (argument == null ||
                argument.RefOrOutKeyword.IsKind(SyntaxKind.None))
            {
                return newProgramState;
            }

            sv = new SymbolicValue();
            newProgramState = newProgramState
                .PopValue()
                .PushValue(sv);
            return StoreSymbolicValueIfSymbolIsTracked(symbol, sv, newProgramState);
        }

        private ProgramState VisitPostfixIncrement(PostfixUnaryExpressionSyntax unary, ProgramState programState)
        {
            var symbol = GetSymbol(unary.Operand);

            return StoreSymbolicValueIfSymbolIsTracked(symbol, new SymbolicValue(), programState);
        }

        private ProgramState VisitPrefixIncrement(PrefixUnaryExpressionSyntax unary, ProgramState programState)
        {
            var symbol = GetSymbol(unary.Operand);
            var sv = new SymbolicValue();
            var newProgramState = programState
                .PopValue()
                .PushValue(sv);

            return StoreSymbolicValueIfSymbolIsTracked(symbol, sv, newProgramState);
        }

        private ProgramState VisitOpAssignment(AssignmentExpressionSyntax assignment, ProgramState programState)
        {
            var sv = new SymbolicValue();
            var newProgramState = programState
                .PopValues(2)
                .PushValue(sv);
            var leftSymbol = GetSymbol(assignment.Left);

            return StoreSymbolicValueIfSymbolIsTracked(leftSymbol, sv, newProgramState);
        }

        private ProgramState VisitSimpleAssignment(AssignmentExpressionSyntax assignment, ProgramState programState)
        {
            SymbolicValue sv;
            var newProgramState = programState.PopValue(out sv);
            if (!CSharpControlFlowGraphBuilder.IsAssignmentWithSimpleLeftSide(assignment))
            {
                newProgramState = newProgramState.PopValue();
            }

            var leftSymbol = GetSymbol(assignment.Left);
            if (leftSymbol.IsNullable())
            {
                sv = new NullableSymbolicValue(sv);
            }

            newProgramState = newProgramState.PushValue(sv);

            return StoreSymbolicValueIfSymbolIsTracked(leftSymbol, sv, newProgramState);
        }

        private ProgramState VisitVariableDeclarator(VariableDeclaratorSyntax declarator, ProgramState programState)
        {
            if (declarator.Initializer?.Value == null)
            {
                return programState;
            }

            SymbolicValue sv;
            var newProgramState = programState.PopValue(out sv);

            var leftSymbol = GetDeclaredSymbol(declarator);
            var rightSymbol = GetSymbol(declarator.Initializer.Value);

            if (leftSymbol == null ||
                (rightSymbol == null && sv == null) ||
                !ExplodedGraphWalker.IsSymbolTracked(leftSymbol))
            {
                return programState;
            }

            if (leftSymbol.IsNullable() &&
                (!rightSymbol.IsNullable() || (rightSymbol == null && sv != null)))
            {
                sv = new NullableSymbolicValue(sv);
            }

            return newProgramState.StoreSymbolicValue(leftSymbol, sv);
        }

        #endregion

        public ProgramState StoreSymbolicValueIfSymbolIsTracked(ISymbol symbol, SymbolicValue symbolicValue,
            ProgramState programState)
        {
            return ExplodedGraphWalker.IsSymbolTracked(symbol)
                ? programState.StoreSymbolicValue(symbol, symbolicValue)
                : programState;
        }
    }
}

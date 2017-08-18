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
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis.CSharp
{
    internal sealed class DisposableConstraintDecorator : ConstraintDecorator
    {
        private static readonly ISet<KnownType> typesDisposingUnderlyingStream = new HashSet<KnownType>
        {
            KnownType.System_IO_StreamReader,
            KnownType.System_IO_StreamWriter
        };

        public DisposableConstraintDecorator(AbstractExplodedGraphWalker explodedGraphWalker)
            : base(explodedGraphWalker)
        {
        }

        public override ProgramState PostProcessInstruction(ExplodedGraphNode node, ProgramState programState)
        {
            switch (node.Instruction.Kind())
            {
                case SyntaxKind.InvocationExpression:
                    return VisitInvocationExpression((InvocationExpressionSyntax)node.Instruction, programState);
                default:
                    return programState;
            }
        }

        public override ProgramState PostProcessBlock(Block block, ProgramState programState)
        {
            var usingEndBlock = block as UsingEndBlock;
            if (usingEndBlock != null)
            {
                var streamIdentifiers = GetUsedStreamIdentifiers((SyntaxNode)
                    usingEndBlock.UsingStatement.Expression ??
                    usingEndBlock.UsingStatement.Declaration);

                return usingEndBlock.Identifiers
                    .Select(syntaxNode => syntaxNode.Parent)
                    .Union(streamIdentifiers)
                    .Aggregate(programState, SetDisposed);
            }

            return programState;
        }

        private ProgramState SetDisposed(ProgramState programState, SyntaxNode syntaxNode) =>
            SetConstraint(programState, syntaxNode, DisposableConstraint.Disposed);

        private ProgramState SetDisposed(ProgramState programState, SymbolicValue symbolicValue) =>
            symbolicValue.SetConstraint(DisposableConstraint.Disposed, programState);

        private IEnumerable<SyntaxNode> GetUsedStreamIdentifiers(SyntaxNode usingExpression) =>
            usingExpression.DescendantNodes()
                .OfType<ObjectCreationExpressionSyntax>()
                .Where(IsStreamDisposingType)
                .Select(FirstArgumentOrDefault)
                .WhereNotNull();

        private static ArgumentSyntax FirstArgumentOrDefault(ObjectCreationExpressionSyntax objectCreation) =>
            objectCreation.ArgumentList?.Arguments.FirstOrDefault();

        private bool IsStreamDisposingType(ObjectCreationExpressionSyntax objectCreation) =>
            GetSymbol(objectCreation.Type)
                .GetSymbolType()
                .DerivesOrImplementsAny(typesDisposingUnderlyingStream);

        private ProgramState VisitInvocationExpression(InvocationExpressionSyntax instruction, ProgramState programState)
        {
            var methodSymbol = GetSymbol(instruction) as IMethodSymbol;
            if (methodSymbol.IsIDisposableDispose())
            {
                var methodIdentifier = instruction.Expression as IdentifierNameSyntax;
                if (methodIdentifier != null)
                {
                    return SetDisposed(programState, SymbolicValue.This);
                }

                var memberAccessExpression = instruction.Expression as MemberAccessExpressionSyntax;
                if (memberAccessExpression?.Expression != null)
                {
                    return SetDisposed(programState, memberAccessExpression.Expression);
                }
            }

            return programState;
        }
    }
}

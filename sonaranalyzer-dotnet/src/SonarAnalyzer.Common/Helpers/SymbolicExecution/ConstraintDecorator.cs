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

namespace SonarAnalyzer.Helpers.FlowAnalysis.Common
{
    internal class ConstraintDecorator
    {
        protected AbstractExplodedGraphWalker ExplodedGraphWalker { get; }
        protected SemanticModel SemanticModel { get; }

        internal ConstraintDecorator(AbstractExplodedGraphWalker explodedGraphWalker)
        {
            ExplodedGraphWalker = explodedGraphWalker;
            SemanticModel = explodedGraphWalker.SemanticModel;
        }

        protected ISymbol GetSymbol(SyntaxNode syntaxNode) =>
            SemanticModel.GetDeclaredSymbol(syntaxNode) ??
            SemanticModel.GetSymbolInfo(syntaxNode).Symbol;

        protected ProgramState SetConstraint(SymbolicValue symbolicValue, SymbolicValueConstraint constraint,
            SyntaxNode syntaxNode, ProgramState programState)
        {
            ExplodedGraphWalker.Publish(
                new ConstraintAdding(symbolicValue, constraint, syntaxNode, programState));

            var newProgramState = symbolicValue.SetConstraint(constraint, programState);

            ExplodedGraphWalker.Publish(
                new ConstraintAdded(symbolicValue, constraint, syntaxNode, newProgramState, programState));

            return newProgramState;
        }

        public virtual ProgramState PostProcessDeclarationParameters(IParameterSymbol symbol,
            SymbolicValue symbolicValue, ProgramState programState)
            => programState;

        public virtual ProgramState PreProcessInstruction(ExplodedGraphNode node, ProgramState programState)
            => programState;

        public virtual ProgramState PostProcessInstruction(ExplodedGraphNode node, ProgramState programState)
            => programState;

        public virtual ProgramState PreProcessBlock(Block block, ProgramState programState)
            => programState;

        public virtual ProgramState PostProcessBlock(Block block, ProgramState programState)
            => programState;
    }
}

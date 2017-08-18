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
        protected readonly AbstractExplodedGraphWalker explodedGraphWalker;
        protected readonly SemanticModel semanticModel;

        internal ConstraintDecorator(AbstractExplodedGraphWalker explodedGraphWalker)
        {
            this.explodedGraphWalker = explodedGraphWalker;
            this.semanticModel = explodedGraphWalker.SemanticModel;
        }

        protected ISymbol GetSymbol(SyntaxNode syntaxNode) =>
            this.semanticModel.GetDeclaredSymbol(syntaxNode) ??
            this.semanticModel.GetSymbolInfo(syntaxNode).Symbol;

        private void OnConstraintApplying(ProgramState programState, SyntaxNode syntaxNode,
            SymbolicValueConstraint constraint)
        {
            ////explodedGraphWalker.InvokeListeners();
        }

        private void OnConstraintApplied(ProgramState newProgramState, SyntaxNode syntaxNode,
            SymbolicValueConstraint constraint)
        {
            ////explodedGraphWalker.InvokeListeners();
        }

        protected ProgramState SetConstraint(ProgramState programState, SyntaxNode syntaxNode,
            SymbolicValueConstraint constraint)
        {
            var symbol = GetSymbol(syntaxNode);

            OnConstraintApplying(programState, syntaxNode, constraint);
            // TODO: FIX ME
            var newProgramState = programState;
            //var newProgramState = symbol.SetConstraint(constraint, programState);
            OnConstraintApplied(newProgramState, syntaxNode, constraint);

            return newProgramState;
        }

        public virtual ProgramState PostProcessDeclarationParameters(IParameterSymbol symbol,
            SymbolicValue symbolicValue, ProgramState programState)
            => programState;

        public virtual ProgramState PreProcessInstruction(SyntaxNode instruction, ProgramState programState)
            => programState;

        public virtual ProgramState PostProcessInstruction(SyntaxNode instruction, ProgramState preProgramState,
            ProgramState postProgramState)
            => postProgramState;

        public virtual ProgramState PreProcessBlock(Block block, ProgramState programState)
            => programState;

        public virtual ProgramState PostProcessBlock(Block block, ProgramState programState)
            => programState;
    }
}

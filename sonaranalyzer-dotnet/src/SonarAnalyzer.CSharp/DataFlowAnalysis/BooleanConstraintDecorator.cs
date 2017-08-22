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

using Microsoft.CodeAnalysis.CSharp;
using SonarAnalyzer.DataFlowAnalysis;

namespace SonarAnalyzer.DataFlowAnalysis.CSharp
{
    internal class BooleanConstraintDecorator : ConstraintDecorator
    {
        public BooleanConstraintDecorator(CSharpExplodedGraphWalker explodedGraphWalker)
            : base(explodedGraphWalker)
        {
        }

        public override ProgramState PostProcessInstruction(ExplodedGraphNode node, ProgramState programState)
        {
            var newProgramState = programState;

            switch (node.Instruction.Kind())
            {
                case SyntaxKind.TrueLiteralExpression:
                case SyntaxKind.FalseLiteralExpression:
                    break; // Constant literal expressions are already handled by exploded graph walker

                case SyntaxKind.IsExpression:
                    {
                        SymbolicValue argSV;
                        node.ProgramState.PopValue(out argSV);
                        if (newProgramState.HasConstraint(argSV, ObjectConstraint.Null))
                        {
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = newProgramState.SetConstraint(sv, BoolConstraint.False);
                        }
                        break;
                    }
            }

            return newProgramState;
        }
    }
}

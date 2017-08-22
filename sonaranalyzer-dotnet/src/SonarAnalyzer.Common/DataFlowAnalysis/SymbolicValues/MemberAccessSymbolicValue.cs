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

namespace SonarAnalyzer.Helpers.FlowAnalysis.Common
{
    public class MemberAccessSymbolicValue : SymbolicValue
    {
        private readonly string memberName;

        public SymbolicValue MemberExpression { get; }

        public MemberAccessSymbolicValue(SymbolicValue memberExpression, string memberName)
        {
            MemberExpression = memberExpression;
            this.memberName = memberName;
        }

        public override string ToString()
        {
            return $"{MemberExpression}.{memberName}";
        }

        public override IEnumerable<ProgramState> TrySetConstraint(SymbolicValueConstraint constraint,
            ProgramState currentProgramState)
        {
            // TODO: Move this somewhere else
            if (MemberExpression is NullableSymbolicValue)
            {
                if (memberName == "HasValue" &&
                    constraint is BoolConstraint)
                {
                    var oldConstraint = currentProgramState.Constraints.GetValueOrDefault(MemberExpression)
                        ?.GetConstraintOrDefault<NullableValueConstraint>();
                    if (oldConstraint != null)
                    {
                        var isImpossibleState =
                            (oldConstraint == NullableValueConstraint.HasValue && constraint == BoolConstraint.False) ||
                            (oldConstraint == NullableValueConstraint.NoValue && constraint == BoolConstraint.True);
                        return isImpossibleState
                            ? Enumerable.Empty<ProgramState>()
                            : new[] { currentProgramState };
                    }
                }

                return MemberExpression.TrySetConstraint(constraint, currentProgramState);
            }

            return base.TrySetConstraint(constraint, currentProgramState);
        }
    }
}

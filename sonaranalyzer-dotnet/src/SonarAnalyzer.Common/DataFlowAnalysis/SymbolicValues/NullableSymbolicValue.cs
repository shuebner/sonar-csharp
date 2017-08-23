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

using System;
using System.Collections.Generic;
using System.Linq;

namespace SonarAnalyzer.DataFlowAnalysis
{
    public class NullableSymbolicValue : SymbolicValue
    {
        private const string HasValueMember = nameof(Nullable<int>.HasValue);

        public SymbolicValue WrappedSymbolicValue { get; }

        public NullableSymbolicValue(SymbolicValue wrappedSymbolicValue)
        {
            WrappedSymbolicValue = wrappedSymbolicValue;
        }

        public override bool CanHandleMemberAccess(string memberName)
        {
            return memberName == HasValueMember;
        }

        public override IEnumerable<ProgramState> HandleMemberAccess(SymbolicValueConstraint memberConstraint, 
            string memberName, ProgramState programState)
        {
            if (memberName == HasValueMember)
            {
                if (!(memberConstraint is BoolConstraint))
                {
                    throw new ArgumentOutOfRangeException(nameof(memberConstraint),
                        $"Only BoolConstraint is supported for '{HasValueMember}'");
                }

                var constraint = memberConstraint == BoolConstraint.True
                    ? NullableValueConstraint.HasValue
                    : NullableValueConstraint.NoValue;

                return TrySetConstraint(constraint, programState);
            }

            throw new InvalidOperationException(
                $"Cannot handle '{memberName}'. Either implement handler or update the CanHandleMemberAccess method.");
        }

        public override IEnumerable<ProgramState> TrySetConstraint(SymbolicValueConstraint constraint,
            ProgramState programState)
        {
            // TODO: should this be argument exception?
            if (constraint == null)
            {
                return new[] { programState };
            }

            if (constraint is ObjectConstraint)
            {
                var optionalConstraint = constraint == ObjectConstraint.Null
                    ? NullableValueConstraint.NoValue
                    : NullableValueConstraint.HasValue;

                return base.TrySetConstraint(optionalConstraint, programState);
            }

            if (constraint is NullableValueConstraint)
            {
                return base.TrySetConstraint(constraint, programState);
            }

            return TrySetConstraint(NullableValueConstraint.HasValue, programState)
                .SelectMany(ps => WrappedSymbolicValue.TrySetConstraint(constraint, ps));
        }

        public override IEnumerable<ProgramState> TrySetOppositeConstraint(SymbolicValueConstraint constraint, ProgramState programState)
        {
            var negateConstraint = constraint?.OppositeForLogicalNot;

            if (constraint is BoolConstraint)
            {
                return TrySetConstraint(negateConstraint, programState)
                  .Union(TrySetConstraint(NullableValueConstraint.NoValue, programState));
            }

            return TrySetConstraint(negateConstraint, programState);
        }

        public override string ToString()
        {
            return $"NULLABLE_SV_{base.identifier}({WrappedSymbolicValue?.ToString()})";
        }
    }
}

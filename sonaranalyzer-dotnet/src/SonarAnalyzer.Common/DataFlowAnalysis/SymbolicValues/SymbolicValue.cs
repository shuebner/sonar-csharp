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
    public class SymbolicValue
    {
        public static readonly SymbolicValue True = new SymbolicValue("TRUE");
        public static readonly SymbolicValue False = new SymbolicValue("FALSE");
        public static readonly SymbolicValue Null = new SymbolicValue("NULL");
        public static readonly SymbolicValue This = new SymbolicValue("THIS");
        public static readonly SymbolicValue Base = new SymbolicValue("BASE");

        protected readonly string identifier;

        private static int SymbolicValueCounter = 0;

        internal SymbolicValue()
            : this((SymbolicValueCounter++).ToString())
        {
        }

        private SymbolicValue(string identifier)
        {
            this.identifier = identifier;
        }

        public override string ToString()
        {
            if (identifier != null)
            {
                return "SV_" + identifier;
            }

            return base.ToString();
        }

        public virtual bool CanHandleMemberAccess(string memberName) => false;

        public virtual IEnumerable<ProgramState> HandleMemberAccess(SymbolicValueConstraint constraint, string memberName,
            ProgramState programState)
        {
            throw new InvalidOperationException(
                $"'{nameof(CanHandleMemberAccess)}' and '{nameof(HandleMemberAccess)}' must be overriden in pairs.");
        }

        protected IEnumerable<ProgramState> ThrowIfTooMany(IEnumerable<ProgramState> states)
        {
            var stateList = states.ToList();
            if (stateList.Count >= AbstractExplodedGraphWalker.MaxInternalStateCount)
            {
                throw new TooManyInternalStatesException();
            }

            return stateList;
        }

        public virtual IEnumerable<ProgramState> TrySetConstraint(SymbolicValueConstraint constraint,
            ProgramState programState)
        {
            // TODO: should this be argument exception?
            if (constraint == null)
            {
                return new[] { programState };
            }

            var oldConstraint = programState.GetConstraint(this, constraint.GetType());
            if (oldConstraint == null)
            {
                return new[] { programState.SetConstraint(this, constraint) };
            }

            if (oldConstraint != constraint)
            {
                return Enumerable.Empty<ProgramState>();
            }

            return new[] { programState };
        }

        public virtual IEnumerable<ProgramState> TrySetOppositeConstraint(SymbolicValueConstraint constraint,
            ProgramState programState)
        {
            return TrySetConstraint(constraint?.OppositeForLogicalNot, programState);
        }

        public IEnumerable<ProgramState> TrySetConstraints(SymbolicValueConstraints constraints,
            ProgramState programState)
        {
            return TrySetConstraints(constraints, programState, false);
        }

        public IEnumerable<ProgramState> TrySetOppositeConstraints(SymbolicValueConstraints constraints,
            ProgramState programState)
        {
            return TrySetConstraints(constraints, programState, true);
        }

        private IEnumerable<ProgramState> TrySetConstraints(SymbolicValueConstraints constraints,
            ProgramState programState, bool isOppositeConstraints)
        {
            IEnumerable<ProgramState> programStates = new [] { programState };

            if (constraints == null)
            {
                return programStates;
            }

            foreach (var constraint in constraints.GetConstraints())
            {
                programStates = programStates.SelectMany(ps =>
                    isOppositeConstraints
                    ? TrySetOppositeConstraint(constraint, ps)
                    : TrySetConstraint(constraint, ps));
            }

            return programStates;
        }
    }
}

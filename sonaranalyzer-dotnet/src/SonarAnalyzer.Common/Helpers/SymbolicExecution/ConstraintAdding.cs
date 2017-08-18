using Microsoft.CodeAnalysis;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis
{
    public class ConstraintAdding
    {
        public ProgramState ProgramState { get; }
        public SyntaxNode Instruction { get; }
        public SymbolicValue SymbolicValue { get; }
        public SymbolicValueConstraint Constraint { get; }

        public ConstraintAdding(ProgramState programState, SyntaxNode instruction, SymbolicValue symbolicValue,
            SymbolicValueConstraint constraint)
        {
            ProgramState = programState;
            Instruction = instruction;
            SymbolicValue = symbolicValue;
            Constraint = constraint;
        }
    }

    public class ConstraintAdded
    {
        public ProgramState ProgramState { get; }
        public SyntaxNode Instruction { get; }
        public SymbolicValue SymbolicValue { get; }
        public SymbolicValueConstraint Constraint { get; }
        public ProgramState PreviousProgramState { get; }

        public ConstraintAdded(ProgramState programState, SyntaxNode instruction, SymbolicValue symbolicValue,
            SymbolicValueConstraint constraint, ProgramState previousProgramState)
        {
            ProgramState = programState;
            Instruction = instruction;
            SymbolicValue = symbolicValue;
            Constraint = constraint;
            PreviousProgramState = previousProgramState;
        }
    }
}

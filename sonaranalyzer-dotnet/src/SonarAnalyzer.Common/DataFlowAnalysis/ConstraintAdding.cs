using Microsoft.CodeAnalysis;
using SonarAnalyzer.DataFlowAnalysis;

namespace SonarAnalyzer.DataFlowAnalysis
{
    public class ConstraintAdding
    {
        public ProgramState ProgramState { get; }
        public SyntaxNode Instruction { get; }
        public SymbolicValue SymbolicValue { get; }
        public SymbolicValueConstraint Constraint { get; }

        public ConstraintAdding(SymbolicValue symbolicValue, SymbolicValueConstraint constraint, SyntaxNode instruction,
            ProgramState programState)
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

        public ConstraintAdded(SymbolicValue symbolicValue, SymbolicValueConstraint constraint, SyntaxNode instruction,
            ProgramState programState, ProgramState previousProgramState)
        {
            ProgramState = programState;
            Instruction = instruction;
            SymbolicValue = symbolicValue;
            Constraint = constraint;
            PreviousProgramState = previousProgramState;
        }
    }
}

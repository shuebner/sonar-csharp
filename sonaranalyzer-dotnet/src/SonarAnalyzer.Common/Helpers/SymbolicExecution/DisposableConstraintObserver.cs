using System;
using Microsoft.CodeAnalysis;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis
{
    public class DisposableConstraintObserver : ConstraintObserver
    {
        public Action<SyntaxNode> Report { get; }

        public DisposableConstraintObserver(Action<SyntaxNode> report)
        {
            Report = report;
        }

        public override void OnNext(ConstraintAdding value)
        {
            if (value.Constraint == DisposableConstraint.Disposed &&
                value.SymbolicValue.HasConstraint(DisposableConstraint.Disposed, value.ProgramState))
            {
                Report(value.Instruction);
            }
        }
    }
}

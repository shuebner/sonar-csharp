using System;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis
{
    public class DisposableConstraintObserver : ConstraintObserver
    {
        public Action<ConstraintAdding> Report { get; }

        public DisposableConstraintObserver(Action<ConstraintAdding> report)
        {
            Report = report;
        }

        public override void OnNext(ConstraintAdding value)
        {
            if (value.Constraint == DisposableConstraint.Disposed &&
                value.SymbolicValue.HasConstraint(DisposableConstraint.Disposed, value.ProgramState))
            {
                Report(value);
            }
        }
    }
}

using System;

namespace SonarAnalyzer.Helpers.FlowAnalysis
{
    public class ConstraintObserver : IObserver<ConstraintAdded>, IObserver<ConstraintAdding>
    {
        public void OnCompleted()
        {
            // we don't care
        }

        public void OnError(Exception error)
        {
            // we don't care
        }

        public virtual void OnNext(ConstraintAdded value)
        {
        }

        public virtual void OnNext(ConstraintAdding value)
        {
        }
    }
}

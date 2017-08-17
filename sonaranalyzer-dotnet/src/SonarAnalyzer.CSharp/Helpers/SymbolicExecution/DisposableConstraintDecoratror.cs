using System;
using Microsoft.CodeAnalysis;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis.CSharp
{
    public class DisposableConstraintDecoratror : IDomainConstraintDecorator
    {
        public ProgramState PreProcessInstruction(SyntaxNode instruction, ProgramState programState)
        {
            throw new NotImplementedException();
        }

        public ProgramState PostProcessInstruction(SyntaxNode instruction, ProgramState programState)
        {
            throw new NotImplementedException();
        }
    }
}

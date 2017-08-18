using System;
using Microsoft.CodeAnalysis;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis.CSharp
{
    public class CollectionConstraintDecoratror : IDomainConstraintDecorator
    {
        public ProgramState PreProcessInstruction(SyntaxNode instruction, ProgramState programState)
        {
            throw new NotImplementedException();
        }

        public ProgramState PostProcessInstruction(SyntaxNode instruction, ProgramState preProgramState,
            ProgramState postProgramState)
        {
            throw new NotImplementedException();
        }
    }
}

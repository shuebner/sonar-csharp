using Microsoft.CodeAnalysis;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis.CSharp
{
    public interface IDomainConstraintDecorator
    {
        ProgramState PreProcessInstruction(SyntaxNode instruction, ProgramState programState);
        ProgramState PostProcessInstruction(SyntaxNode instruction, ProgramState programState);
    }
}

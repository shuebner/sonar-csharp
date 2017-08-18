using System;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using SonarAnalyzer.Helpers.FlowAnalysis.Common;

namespace SonarAnalyzer.Helpers.FlowAnalysis.CSharp
{
    public class BooleanConstraintDecorator : IDomainConstraintDecorator
    {
        private readonly SemanticModel semanticModel;

        public BooleanConstraintDecorator(SemanticModel semanticModel)
        {
            this.semanticModel = semanticModel;
        }

        public ProgramState PreProcessInstruction(SyntaxNode instruction, ProgramState programState)
        {
            throw new NotImplementedException();
        }

        public ProgramState PostProcessInstruction(SyntaxNode instruction, ProgramState preProgramState,
            ProgramState postProgramState)
        {
            var newProgramState = postProgramState;

            switch (instruction.Kind())
            {
                case SyntaxKind.TrueLiteralExpression:
                case SyntaxKind.FalseLiteralExpression:
                    break; // Constant literal expressions are already handled by exploded graph walker

                case SyntaxKind.IsExpression:
                    {
                        SymbolicValue argSV;
                        preProgramState.PopValue(out argSV);
                        if (argSV.HasConstraint(ObjectConstraint.Null, newProgramState))
                        {
                            var sv = newProgramState.ExpressionStack.Peek();
                            newProgramState = sv.SetConstraint(BoolConstraint.False, newProgramState);
                        }
                        break;
                    }
            }

            return newProgramState;
        }
    }
}

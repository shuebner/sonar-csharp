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

using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;
using SonarAnalyzer.Helpers.FlowAnalysis;
using SonarAnalyzer.Helpers.FlowAnalysis.CSharp;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public class ValuePropertyShouldNotBeCalledOnEmptyNullable : SonarDiagnosticAnalyzer
    {
        internal const string DiagnosticId = "S3655";
        private const string MessageFormat = "'{0}' has no value on at least one execution path.";

        private static readonly DiagnosticDescriptor rule =
            DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(rule);

        private const string ValueLiteral = "Value";
        private const string HasValueLiteral = "HasValue";

        protected sealed override void Initialize(SonarAnalysisContext context)
        {
            context.RegisterExplodedGraphBasedAnalysis(CheckEmptyNullableValueAccess);
        }

        private static void CheckEmptyNullableValueAccess(CSharpExplodedGraphWalker explodedGraph,
            SyntaxNodeAnalysisContext context)
        {
            explodedGraph.Subscribe(new NullableConstraintObserver(args =>
                context.ReportDiagnostic(Diagnostic.Create(rule, args.Instruction.GetLocation(), args.Instruction))));

            explodedGraph.Walk();
        }
    }
}
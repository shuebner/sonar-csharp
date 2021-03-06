/*
 * SonarC#
 * Copyright (C) 2014-2017 SonarSource SA
 * mailto:info AT sonarsource DOT com
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
package org.sonar.plugins.csharp;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.InputFile.Type;
import org.sonar.api.batch.rule.ActiveRule;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.rule.RuleKey;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;
import org.sonarsource.dotnet.shared.plugins.ProtobufDataImporter;
import org.sonarsource.dotnet.shared.plugins.ReportPathCollector;
import org.sonarsource.dotnet.shared.plugins.RoslynDataImporter;

import static java.util.stream.Collectors.toList;

public class CSharpSensor implements Sensor {

  private static final Logger LOG = Loggers.get(CSharpSensor.class);

  private final ProtobufDataImporter protobufDataImporter;
  private final RoslynDataImporter roslynDataImporter;
  private final ReportPathCollector reportPathCollector;

  public CSharpSensor(ReportPathCollector reportPathCollector, ProtobufDataImporter protobufDataImporter, RoslynDataImporter roslynDataImporter) {
    this.reportPathCollector = reportPathCollector;
    this.protobufDataImporter = protobufDataImporter;
    this.roslynDataImporter = roslynDataImporter;
  }

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor.name("C#")
      .onlyOnLanguage(CSharpPlugin.LANGUAGE_KEY)
      .global();
  }

  @Override
  public void execute(SensorContext context) {
    if (shouldExecuteOnProject(context.fileSystem())) {
      executeInternal(context);
    }
  }

  private static boolean shouldExecuteOnProject(FileSystem fs) {
    if (!filesToAnalyze(fs).iterator().hasNext()) {
      LOG.debug("No files to analyze. Skip Sensor.");
      return false;
    }

    return true;
  }

  private static Iterable<InputFile> filesToAnalyze(FileSystem fs) {
    return fs.inputFiles(fs.predicates().and(fs.predicates().hasType(Type.MAIN), fs.predicates().hasLanguage(CSharpPlugin.LANGUAGE_KEY)));
  }

  private void executeInternal(SensorContext context) {
    List<Path> protobufPaths = reportPathCollector.protobufDirs();
    if (!protobufPaths.isEmpty()) {
      protobufDataImporter.importResults(context, protobufPaths);
    }

    List<Path> roslynDirs = reportPathCollector.roslynDirs();
    if (!roslynDirs.isEmpty()) {
      Map<String, List<RuleKey>> activeRoslynRulesByPartialRepoKey = RoslynProfileExporter.activeRoslynRulesByPartialRepoKey(context.activeRules()
        .findAll()
        .stream()
        .map(ActiveRule::ruleKey)
        .collect(toList()));
      roslynDataImporter.importRoslynReports(roslynDirs, context, activeRoslynRulesByPartialRepoKey);
    }
  }
}

/*
 * SonarSource :: .NET :: Shared library
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
package org.sonarsource.dotnet.shared.plugins;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.sonar.api.config.PropertyDefinitions;
import org.sonar.api.config.internal.MapSettings;
import org.sonar.api.utils.log.LogTester;
import org.sonar.api.utils.log.LoggerLevel;

import static org.assertj.core.api.Assertions.assertThat;

public class AbstractConfigurationTest {
  @Rule
  public TemporaryFolder temp = new TemporaryFolder();
  @Rule
  public LogTester logTester = new LogTester();

  private Path workDir;
  private MapSettings settings;

  private AbstractConfiguration config;

  @Before
  public void setUp() {
    workDir = temp.getRoot().toPath();
    AbstractPropertyDefinitions definitions = new AbstractPropertyDefinitions("cs", "cs") {
    };
    settings = new MapSettings(new PropertyDefinitions(definitions.create()));
  }

  private Path createProtobufOut(String name) throws IOException {
    Path path = workDir.resolve(name);
    Path outputCs = path.resolve("output-cs");
    Files.createDirectories(outputCs);
    Files.createFile(outputCs.resolve("dummy.pb"));
    return path;
  }

  private void setProtobufOut() throws IOException {
    Path path1 = createProtobufOut("report1");
    Path path2 = createProtobufOut("report2");
    settings.setProperty("sonar.cs.analyzer.projectOutPaths", new String[] {path1.toString(), path2.toString()});
  }

  private void setOldProtobufOut() throws IOException {
    Path path = createProtobufOut("report");
    settings.setProperty("sonar.cs.analyzer.projectOutPath", path.toString());
  }

  private void createOldRoslynOut() throws IOException {
    Path path = temp.newFile("roslyn-report.json").toPath();
    settings.setProperty("sonar.cs.roslyn.reportFilePath", path.toString());
  }

  private void createRoslynOut() throws IOException {
    Path path = temp.newFile("roslyn-report.json").toPath();
    Path path2 = temp.newFile("roslyn-report2.json").toPath();
    settings.setProperty("sonar.cs.roslyn.reportFilePaths", new String[] {path.toString(), path2.toString()});
  }

  @Test
  public void onlyNewRoslynReportPresent() throws IOException {
    createRoslynOut();
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPathsSilent()).isEmpty();
    assertThat(config.roslynReportPaths()).containsOnly(workDir.resolve("roslyn-report.json"), workDir.resolve("roslyn-report2.json"));
  }

  @Test
  public void onlyOldRoslynReportPresent() throws IOException {
    createOldRoslynOut();
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPathsSilent()).isEmpty();
    assertThat(config.roslynReportPaths()).containsOnly(workDir.resolve("roslyn-report.json"));
  }

  @Test
  public void giveWarningsWhenGettingProtobufPathAndNoPropertyAvailable() {
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPaths()).isEmpty();
    assertThat(logTester.logs(LoggerLevel.WARN)).containsOnly("Property missing: 'sonar.cs.analyzer.projectOutPaths'. No protobuf files will be loaded for this project.");
  }

  @Test
  public void giveWarningsWhenGettingProtobufPathAndNoFolderAvailable() throws IOException {
    Path path1 = createProtobufOut("report");
    settings.setProperty("sonar.cs.analyzer.projectOutPaths", new String[] {path1.toString(), "non-existing"});
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPaths()).containsOnly(path1.resolve("output-cs"));
    assertThat(logTester.logs(LoggerLevel.WARN)).hasSize(1);
    assertThat(logTester.logs(LoggerLevel.WARN).get(0)).matches(s -> s.startsWith("Analyzer working directory does not exist"));
  }

  @Test
  public void giveWarningsWhenGettingOldProtobufPathAndNoFolderAvailable() {
    settings.setProperty("sonar.cs.analyzer.projectOutPath", "non-existing");
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPaths()).isEmpty();
    assertThat(logTester.logs(LoggerLevel.WARN)).hasSize(1);
    assertThat(logTester.logs(LoggerLevel.WARN).get(0)).matches(s -> s.startsWith("Analyzer working directory does not exist"));
  }

  @Test
  public void informHowManyProtoFilesAreFound() throws IOException {
    setProtobufOut();
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPaths()).isNotEmpty();
    assertThat(logTester.logs(LoggerLevel.DEBUG)).hasSize(2);
    assertThat(logTester.logs(LoggerLevel.WARN)).allMatch(s -> s.endsWith("contains no .pb file(s). No protobuf files will be loaded from this directory."));
  }

  @Test
  public void giveWarningsWhenGettingProtobufPathAndFolderIsEmpty() throws IOException {
    Path path = workDir.resolve("report");
    Path outputCs = path.resolve("output-cs");
    Files.createDirectories(outputCs);
    settings.setProperty("sonar.cs.analyzer.projectOutPath", path.toString());
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPaths()).isEmpty();
    assertThat(logTester.logs(LoggerLevel.WARN)).hasSize(1);
    assertThat(logTester.logs(LoggerLevel.WARN).get(0))
      .matches(s -> s.endsWith("contains no .pb file(s). Analyzer results won't be loaded from this directory."));
  }

  @Test
  public void onlyProtobufReportsPresent() throws IOException {
    setProtobufOut();
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPathsSilent()).isNotEmpty();
    assertThat(config.roslynReportPaths()).isEmpty();
    assertThat(config.protobufReportPathsSilent()).containsOnly(workDir.resolve("report1").resolve("output-cs"), workDir.resolve("report2").resolve("output-cs"));
  }

  @Test
  public void onlyOldProtobufReportsPresent() throws IOException {
    setOldProtobufOut();
    config = new AbstractConfiguration(settings.asConfig(), "cs") {
    };
    assertThat(config.protobufReportPathsSilent()).isNotEmpty();
    assertThat(config.roslynReportPaths()).isEmpty();
    assertThat(config.protobufReportPathsSilent()).containsOnly(workDir.resolve("report").resolve("output-cs"));
  }
}

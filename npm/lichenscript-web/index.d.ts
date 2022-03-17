 
 export interface Position {
  line: number;
  character: number;
}

export interface Range {
  /**
   * The range's start position
   */
  start: Position;
  /**
   * The range's end position.
   */
  end: Position;
}

export interface CodeDescription {
  href: string;
}

export interface Location {
  uri: string;
  range: Range;
}

export interface DiagnosticRelatedInformation {
  location: Location;
  message: string;
}

 export interface Diagnostic {
  range: Range;
  severity?: number;
  code?: number | string;
  codeDescription?: CodeDescription;
  source?: string;
  message: string;
  tags?: number[];
  relatedInformation?: DiagnosticRelatedInformation[];
  data?: unknown;
}

export interface FSProvider {
  isDirectory(path: string): boolean;
  isFile(path: string): boolean;
  getRealPath(path: string): string;
  lsDir(path: string): string[];
  mkdirRecursive(path: string): void;
  fileExists(path: string): boolean;
  readFileContent(path: string): string;
  writeFileContent(path: string, content: string): void;
}

export interface Config {
  findPaths: string[];
  runtimeDir: string;
  precludeDir: string;
}

export interface IntellisenseInstantce {
  parseAndCache(path: string, content: string): Diagnostic[];
  typecheckDir(path: string): Diagnostic[];
  findDefinition(path: string, offset: number): Location | undefined;
  deleteFile(path: string): void;
}

export function compile(content: string): string;

export function createIntellisenseInstance(
  provider: FSProvider, config: Config): IntellisenseInstantce;

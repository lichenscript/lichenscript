 
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

export declare type CompletionItemKind = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25;

export interface CompletionItem {
  label: string;
  kind?: CompletionItemKind;
}

export interface IntellisenseInstantce {
  parseAndCache(path: string, content: string): Diagnostic[];
  typecheckDir(path: string): Diagnostic[];
  findDefinition(path: string, offset: number): Location | undefined;
  findCompletion(path: string, offset: number): CompletionItem[];
  deleteFile(path: string): void;
}

export interface LichenFile {
  name: string;
  content: string;
}

export interface LichenModule {
  name: string;
  files: LichenFile[];
}

export function registerModule(mod: LichenModule): void;

export interface Program {
  execute(args: string[]): void;
}

export function compile(content: string): Program;

export function createIntellisenseInstance(
  provider: FSProvider, config: Config): IntellisenseInstantce;

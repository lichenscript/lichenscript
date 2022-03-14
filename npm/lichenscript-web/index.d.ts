
export interface FSProvider {
  isDirectory(path: string): boolean;
  isFile(path: string): boolean;
  getRealPath(path: string): string;
  lsDir(path: string): string[];
  mkdirRecursive(path: string): void;
  fileExists(path: string): boolean;
  readFileContent(path: string): string;
  writeFileContent(path: string): void;
}

export interface IntellisenseInstantce {
  parseAndCacheWillThrow(path: string, content: string): void;
  deleteFile(path: string): void;
}

export function compile(content: string): string;

export function createIntellisenseInstance(provider: FSProvider): IntellisenseInstantce;

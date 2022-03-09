
export interface IntellisenseInstantce {
	parseAndCacheWillThrow(path: string, content: string): void;
	deleteFile(path: string): void;
}

export function compile(content: string): string;

export function createIntellisenseInstance(): IntellisenseInstantce;

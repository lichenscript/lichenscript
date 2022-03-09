
export interface IntellisenseInstantce {
	parseAndCacheWillThrow(path: string, content: string);
	deleteFile(path: string);
}

export function compile(content: string): string;

export function createIntellisenseInstance(): IntellisenseInstantce;

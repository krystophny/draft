let _accessToken: string;
let _apiUrl: string;

export function getConfig() {
    return {
        headers: {
            'Private-Token': getAccessToken(),
        },
    };
}

export function initConfig(accessToken: string, hostname: string): void {
    _accessToken = accessToken;
    _apiUrl = `https://${hostname}/api/v4`;
}

export function getAccessToken(): string {
    return _accessToken;
}

export function getApiUrl(): string {
    return _apiUrl;
}

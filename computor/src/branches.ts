import axios from 'axios';
import { getConfig, getApiUrl } from './config';

interface Branch {
    id: string;
    name: string;
    merged: boolean;
    protected: boolean;
    developers_can_push: boolean;
    developers_can_merge: boolean;
    can_push: boolean;
    default: boolean;
}

interface MergeRequest {
    id: string;
    state: string;
    source_branch: string;
    target_branch: string;
    title: string;
}

export async function getOpenBranches(projectId: string): Promise<Branch[]> {
    const url = projectUrl(projectId, 'repository/branches');
    const response = await axios.get<Branch[]>(url, getConfig());
    const openBranches = response.data.filter(branch => !branch.merged);
    return openBranches;
}

export async function getMergeRequests(projectId: string):
    Promise<MergeRequest[]> {
    const url = projectUrl(projectId, 'merge_requests');
    const response = await axios.get<MergeRequest[]>(url, getConfig());
    return response.data
}

export async function createMergeRequest(projectId: string, sourceBranch: string, targetBranch: string, title: string): Promise<MergeRequest> {
    const url = projectUrl(projectId, 'merge_requests');
    const response = await axios.post<MergeRequest>(url, {
        source_branch: sourceBranch,
        target_branch: targetBranch,
        title,
    }, getConfig());
    return response.data;
}

function projectUrl(projectId: string, path: string): string {
    return joinUrl(projectBaseUrl(projectId), path);
}

function projectBaseUrl(projectId: string): string {
    return `${getApiUrl()}/projects/${encodeURIComponent(projectId)}/`;
}

function joinUrl(baseUrl: string, path: string): string {
    return `${baseUrl.replace(/\/$/, '')}/${path.replace(/^\//, '')}`;
}

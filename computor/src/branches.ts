import axios from 'axios';

const GITLAB_API_URL = 'https://gitlab.tugraz.at/api/v4';

interface Branch {
name: string;
merged: boolean;
protected: boolean;
developers_can_push: boolean;
developers_can_merge: boolean;
can_push: boolean;
default: boolean;
}

export async function getOpenBranches(projectId: string, accessToken: string): Promise<Branch[]> {
try {
    const response = await axios.get<Branch[]>(
    `${GITLAB_API_URL}/projects/${encodeURIComponent(projectId)}/repository/branches`,
    {
        headers: {
        'Private-Token': accessToken,
        },
    }
    );

    const openBranches = response.data.filter(branch => !branch.merged);
    return openBranches;
} catch (error) {
    console.error('Error fetching branches from GitLab:', error);
    throw error;
}
}

import { getOpenBranches, getMergeRequests } from './branches';
import * as fs from 'fs';
import { initConfig, getConfig } from './config';

async function main() {
    const hostname = 'gitlab.tugraz.at';
    const projectId = '36643';
    try {
        const accessToken = fs.readFileSync('.token', 'utf-8').trim();
        initConfig(accessToken, hostname);
        console.log(getConfig());
    } catch (error: any) {
        console.error('Error reading token:', error.message);
    }

    try {
        const branches = await getOpenBranches(projectId);
        console.log('Open branches:', branches.map(branch => branch.name).join(', '));
    } catch (error: any) {
        console.error('Error fetching branches:', error.message);
    }

    try {
        const mergeRequests = await getMergeRequests(projectId);
        console.log('Merge requests:', mergeRequests.map(mr => mr.title).join(', '));
    }
    catch (error: any) {
    console.error('Error fetching merge requests:', error.message);
}
}
main();

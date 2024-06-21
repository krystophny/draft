import { getOpenBranches } from './branches';
import * as fs from 'fs';

const projectId = '36643';
const accessToken = fs.readFileSync('.token', 'utf-8').trim();

async function main() {
    try {
        const branches = await getOpenBranches(projectId, accessToken);
        console.log('Open branches:', branches.map(branch => branch.name).join(', '));
    } catch (error: any) {
        console.error('Error fetching branches:', error.message);
    }
}
main();

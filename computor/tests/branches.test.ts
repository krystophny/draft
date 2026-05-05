import axios from 'axios';
import { getOpenBranches } from '../src/branches';
import { initConfig } from '../src/config';

jest.mock('axios');
const mockedAxios = axios as jest.Mocked<typeof axios>;

const projectId = 'my-project';
const accessToken = 'my-access-token';
const hostname = 'gitlab.tugraz.at';
const expectedUrl =
  `https://${hostname}/api/v4/projects/${encodeURIComponent(projectId)}/repository/branches`;
const expectedConfig = {
  headers: { 'Private-Token': accessToken },
};

beforeEach(() => {
  initConfig(accessToken, hostname);
  mockedAxios.get.mockReset();
});

describe('getOpenBranches', () => {
  it('returns only unmerged branches', async () => {
    const branches = [
      { id: '1', name: 'branch1', merged: false, protected: false,
        developers_can_push: true, developers_can_merge: true,
        can_push: true, default: false },
      { id: '2', name: 'branch2', merged: true, protected: false,
        developers_can_push: true, developers_can_merge: true,
        can_push: true, default: false },
    ];
    mockedAxios.get.mockResolvedValueOnce({ data: branches });

    const result = await getOpenBranches(projectId);

    expect(result).toEqual([branches[0]]);
    expect(mockedAxios.get).toHaveBeenCalledWith(expectedUrl, expectedConfig);
  });

  it('propagates errors from axios', async () => {
    const error = new Error('Failed to fetch branches');
    mockedAxios.get.mockRejectedValueOnce(error);

    await expect(getOpenBranches(projectId)).rejects.toThrow(error);
    expect(mockedAxios.get).toHaveBeenCalledWith(expectedUrl, expectedConfig);
  });
});

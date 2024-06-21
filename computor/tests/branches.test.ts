import axios from 'axios';
import { getOpenBranches } from '../src/branches';

jest.mock('axios');

describe('getOpenBranches', () => {
  it('should return open branches', async () => {
    const projectId = 'my-project';
    const accessToken = 'my-access-token';
    const branches = [
      {
        name: 'branch1',
        merged: false,
        protected: false,
        developers_can_push: true,
        developers_can_merge: true,
        can_push: true,
        default: false,
      },
      {
        name: 'branch2',
        merged: true,
        protected: false,
        developers_can_push: true,
        developers_can_merge: true,
        can_push: true,
        default: false,
      },
    ];

    // axios.get.mockResolvedValueOnce({ data: branches });

    const result = await getOpenBranches(projectId, accessToken);

    expect(result).toEqual([branches[0]]);
    expect(axios.get).toHaveBeenCalledWith(
      `https://gitlab.tugraz.at/api/v4/projects/${encodeURIComponent(projectId)}/repository/branches`,
      {
        headers: {
          'Private-Token': accessToken,
        },
      }
    );
  });

  it('should throw an error if fetching branches fails', async () => {
    const projectId = 'my-project';
    const accessToken = 'my-access-token';
    const error = new Error('Failed to fetch branches');

    // axios.get.mockRejectedValueOnce(error);

    await expect(getOpenBranches(projectId, accessToken)).rejects.toThrowError(error);
    expect(axios.get).toHaveBeenCalledWith(
      `https://gitlab.tugraz.at/api/v4/projects/${encodeURIComponent(projectId)}/repository/branches`,
      {
        headers: {
          'Private-Token': accessToken,
        },
      }
    );
  });
});

import assert from 'node:assert/strict';
import { afterEach, beforeEach, describe, it } from 'node:test';
import axios, { type AxiosResponse } from 'axios';
import { getOpenBranches } from '../src/branches';
import { initConfig } from '../src/config';

const projectId = 'my-project';
const accessToken = 'my-access-token';
const hostname = 'gitlab.tugraz.at';
const expectedUrl =
  `https://${hostname}/api/v4/projects/${encodeURIComponent(projectId)}/repository/branches`;
const expectedConfig = {
  headers: { 'Private-Token': accessToken },
};
const originalGet = axios.get;

beforeEach(() => {
  initConfig(accessToken, hostname);
});

afterEach(() => {
  axios.get = originalGet;
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
    let calledWith: [string, typeof expectedConfig] | undefined;
    axios.get = (async (url, config) => {
      calledWith = [url, config as typeof expectedConfig];
      return { data: branches } as AxiosResponse<typeof branches>;
    }) as typeof axios.get;

    const result = await getOpenBranches(projectId);

    assert.deepEqual(result, [branches[0]]);
    assert.deepEqual(calledWith, [expectedUrl, expectedConfig]);
  });

  it('propagates errors from axios', async () => {
    const error = new Error('Failed to fetch branches');
    let calledWith: [string, typeof expectedConfig] | undefined;
    axios.get = (async (url, config) => {
      calledWith = [url, config as typeof expectedConfig];
      throw error;
    }) as typeof axios.get;

    await assert.rejects(getOpenBranches(projectId), error);
    assert.deepEqual(calledWith, [expectedUrl, expectedConfig]);
  });
});

# NetCore.Versions.Checks

[GitHub Marketplace](https://github.com/apps/netcore-versions-checks) | [![Build Status](https://dev.azure.com/arthurrump/NetCore.Versions/_apis/build/status/NetCore.Versions.Checks%20CI?branchName=master)](https://dev.azure.com/arthurrump/NetCore.Versions/_build/latest?definitionId=11&branchName=master)

This GitHub app contains a set of checks to make sure the `releases.json` files in the [dotnet/core](https://github.com/dotnet/core) repo follow a consistent structure and are consistent within itself. This app is really only designed to be installed in [dotnet/core](https://github.com/dotnet/core), but if you - for some reason - have a repository with versioning files that follow the exact same format, feel free to install it.

## Implemented checks

On releases-index.json and {version}/releases.json:

- Index latest release should equal channel latest release
- Index latest release date should equal channel latest release date
- Index latest runtime should equal channel latest runtime
- Index latest sdk should equal channel latest sdk
- Index support phase should equal channel support phase
- Index eol date should equal channel eol date

On the information listed in the channel and its releases:

- Latest release date should be newest in list of releases
- Latest release should be the version of the newest release in the list
- Latest runtime version should be the newest runtime version in the list
- Latest sdk version should be the newest sdk version in the list
- Preview channel should only have preview releases
- Channel with eol-date in the past should have support-phase eol

On a single release:

- Release notes link should be well formed absolute uri
- Request to release notes link should give status code 200 OK
- File urls should be well formed absolute uris

The implementation of these checks can be found in [Checks.fs](/src/NetCore.Versions.Checks/Checks.fs). Please open an issue if you have a suggestion for another check.


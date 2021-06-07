# BadCore

It's bad. Do not use.

---

## Builds

GitHub Actions |
:---: |
[![GitHub Actions](https://github.com/4n3ver/BadCore/workflows/Build%20master/badge.svg)](https://github.com/4n3ver/BadCore/actions?query=branch%3Amain) |
[![Build History](https://buildstats.info/github/chart/4n3ver/BadCore)](https://github.com/4n3ver/BadCore/actions?query=branch%3Amain) |

## NuGet

Package | Stable | Prerelease
--- | --- | ---
BadCore | [![NuGet Badge](https://buildstats.info/nuget/BadCore)](https://www.nuget.org/packages/BadCore/) | [![NuGet Badge](https://buildstats.info/nuget/BadCore?includePreReleases=true)](https://www.nuget.org/packages/BadCore/)

---

### Developing

Make sure the following **requirements** are installed on your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

or

- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers)

---

### Environment Variables

- `CONFIGURATION` will set the [configuration](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x#options) of the dotnet commands.  If not set, it will default to Release.
- `GITHUB_TOKEN` will be used to upload release notes and Nuget packages to GitHub.
  - Be sure to set this before releasing

---

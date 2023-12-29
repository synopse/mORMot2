# The "mORMot GET" (mget) Cross-Platform Downloading Tool

The `mget` command-line tool can retrieve files using HTTP or HTTPS, similar to the well-known homonymous GNU WGet tool, but with some unique features, like optional hash computation or peer-to-peer cache downloading. 

It is based on the `THttpClientSocket.WGet()` process from `mormot.net.client`, and optional peer-to-peer cache process as implemented by `THttpPeerCache` from `mormot.net.server`. So everything you get with this tool is also directly available from you own projects using our framework.

Tested on Windows, Linux, and MacOS.

## Resume Downloads

First of all, if a first download attemp failed, it can resume this aborted download, using `RANGE` headers. So only the remaining data will be retreived, which may be a huge time saver when getting huge files. The partially downloaded file has a `.part` file name extension.

## Hash Verification

A cryptographic hash (typically MD5, SHA1 or SHA256) can be retrieved from the server before getting the file itself, so that it will checked at the end of the download.

You could also supply the hash at the command line level, if you know its value, e.g. from a public web site article.

## Peer-To-Peer Download

On corporate networks, one performance and usuability issue is often the need to download content from the main corportate servers, via a VPN, over the Internet. In some countries, or due to some technical limitations, the bandwith to the main servers may be limited, and become a bottleneck.

Our tool is able to maintain a local cache of already downloaded files (stored by their hash), and ask its peers on its local network if some content is not already in its cache. If the file is found, it will be downloaded locally, without using the main server. Under the hood, a request will be broadcasted over UDP, to discover the presence of a file hash. If nothing is found, the main server will be requested with a GET, as usual. But if some peers do have the requested file, then the best peer will be selected and ask for a local download (over HTTP), with very high performance.

## In Practice

Run `mget` to get the minimal set of information:

```
ab@dev:~/mget$ ./mget
mget 2.1.6572: retrieve files - and more
proudly using mORMot 2 - synopse.info

Usage: mget  <http://uri> [options] [params]

mget --help to display full usage description
```

Run the `mget /help` (on Windows) or `./mget --help` (on POSIX) command to get the list of all options available as command line switches.

The main typical usages are the following:

- `mget https://someuri/some/file.ext` to retrieve a file from an URI, with optional resume if the download was aborted;
- `mget 4544b3...68ba7a@http://someuri/some/file.ext` to retrieve a file from an URI, with the specified hash value (md5/sha1/sha256 algo will be guessed from the hexadecimal hash length);
- `mget https://someuri/some/file.ext /hashAlgo sha256` to retrieve a file from an URI, first retrieving its hash value from `https://someuri/some/file.ext.sha256`;
- `mget --prompt` to ask for an URI or hash@URI from a prompt - finish with a void entry;
- `mget --prompt --peer` to ask for an URI or hash@URI from a prompt, with *PeerCache* enabled.

A lot of additional features or options are available, e.g. use a local cache, limit the bandwith usage during the download, define HTTPS certificate validation, or tune all `--peer` settings.

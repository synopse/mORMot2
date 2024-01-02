# The "mORMot GET" (mget) Cross-Platform Downloading Tool

The `mget` command-line tool can retrieve files using HTTP or HTTPS, similar to the well-known homonymous GNU WGet tool, but with some unique features, like optional hash computation or peer-to-peer cache downloading. 

It is based on the `THttpClientSocket.WGet()` process from `mormot.net.client`, and optional peer-to-peer cache process as implemented by `THttpPeerCache` from `mormot.net.server`. So everything you get with this tool is also directly available from you own projects using our framework.

Tested on Windows, Linux, and MacOS.

## Resume Downloads

First of all, if a first download attempt failed (e.g. the network was interrupted), it can resume this aborted download, using `RANGE` headers. So only the remaining data will be retrieved, which may be a real time saver when getting huge files on weak connection. The partially downloaded file has a `.part` file name extension.

## Hash Verification

A cryptographic hash (typically MD5, SHA1 or SHA256) can be retrieved from the server before getting the file itself, to be checked at the end of the download. On recent 64-bit Intel/AMD, SHA-NI opcodes will be used for fast SHA1 and SHA256 calculation.

You could also supply the hash at the command line level, if you know its value, e.g. from a public web site article.

## Peer-To-Peer Download

### Presentation

On corporate networks, one performance and usability issue is often the need to download content from the main corporate servers, via a VPN, over the Internet. In some countries, or due to some technical limitations, the bandwidth to the main servers may be limited, and become a bottleneck.

Our tool is able to maintain a local cache of already downloaded files (stored by their hash), and ask its peers on the local network if some content is not already in their cache. If the file is found, it will be downloaded locally, without using the main server but for a quick HEAD to ensure the file still exists on the main server (with the expected size).

Under the hood, a request will be broadcasted over UDP, to discover the presence of a file hash. If nothing is found, the main server will be requested with a GET, as usual. But if some peers do have the requested file, then the best peer will be selected and asked for a local download (over HTTP), with very good performance.

### Security Notes

This *PeerCache* mechanism has been designed to be as secured as possible, even with its default settings.
In a nutshell, its internal process expects a "secret" phrase to match on all peers for any communication to happen.

Here are some additional information:
- A global shared secret key is used to cipher and authenticate UDP frames and HTTP requests among all peers. This key should be strong enough and private, and can be provided via `--peerSecret` or `--peerSecretHexa`. It is derived internally using SHA-256 to generate secrets for encryption/authentication over both UDP and HTTP.
- UDP frames are quickly signed with a secret-derivated crc before AES-GCM-128 encoding, so most  attacks would be immediately detected.
- HTTP requests on the local TCP port are also authenticated with a similar AES-GCM-128 bearer.
- Peers which did send invalid requests over UDP or TCP will have their IP banished for a few minutes, to avoid fuzzing or denial of service attacks.
- HTTP content is not encrypted on the wire by default, because it sounds not mandatory on a local network, but the `SelfSignedHttps` option can enable HTTPS if needed.
- Tampering is avoided by using cryptographic hashes for the requests, the local storage and eventually in WGet, which would discard any invalid data.
- The client caches only the content that it has requested itself, to reduce any information disclosure.
- Local cache folders should have the proper ACL file permissions defined.
- Local cached files are not encrypted, so if data leakage is a concern, consider enabling file systems encryption (e.g. BitLocker or Luks).
- Resulting safety is similar to what Microsoft BranchCache offers, with no need of additional servers.

## In Practice

Run `mget` to get the minimal set of information:

```
ab@dev:~/mget$ ./mget
mget 2.1.6576: retrieve files - and more
proudly using mORMot 2 - synopse.info

Usage: mget  <http://uri> [options] [params]

mget --help to display full usage description
```

Run the `mget /help` (on Windows) or `./mget --help` (on POSIX) command to get the list of all options available as command line switches. Note that the switches naming is case-sensitive on all platforms.

The main typical usages are the following:

- `mget https://someuri/some/file.ext` to retrieve a file from an URI, with optional resume if the download was aborted;
- `mget 4544b3...68ba7a@http://someuri/some/file.ext` to retrieve a file from an URI, with the specified hash value (md5/sha1/sha256 algo will be guessed from the hexadecimal hash length);
- `mget https://someuri/some/file.ext --hashAlgo sha256` to retrieve a file from an URI, first retrieving its hash value from `https://someuri/some/file.ext.sha256`;
- `mget --prompt` to ask for an URI or hash@URI from a prompt - terminates on error or from a void entry;
- `mget --prompt --peer` to ask for an URI or hash@URI from a prompt, with *PeerCache* enabled.

By design, *PeerCache* processing needs some UDP and TCP servers to run on the background. This is the point of the `--prompt` command line kind of process: you can ask for files to downloads, but peers can also ask for your own cached files during the prompt wait.

A lot of additional features or options are available, e.g. use a local cache folder, limit the bandwidth usage during the download, define HTTPS certificate validation, or tune any `--peer` setting, like `--peerSecret` or `--peerPort`.

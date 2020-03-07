# mORMot Application and (Micro)Services Units

## Folder Content

This folder holds the (Micro)Services/Daemons and Application high-level features of the *mORMot* Open Source framework, version 2.

## (Micro)Services and Daemons

*Daemons* are the POSIX way of creating background executables, which are called 
*Services* under Windows. Since "Service" is more tied to SOA in our framework, we rather use "Daemon" to refer such background executables, which can easily be implemented as cross-platform by those units.

*MicroServices* are a specific kind self-sufficent and uncoupled SOA servers. *mORMot* excels in implementing such services, as a stand-alone and self-configuring daemons. In particular, SOA endpoints can be implemented via interface-based services, and *SQLite3* local database(s) could be used for local persistence.
# Hisoka

Hisoka (ひそか) or hidden/private in japanese is a minimalist file manager that store file by encrypting them.

## How to use

### - **Initialization**

To start with hisoka, you first need to initialize it.

```
$ hisoka init --help
NAME
       hisoka-init - Initialise hisoka app

SYNOPSIS
       hisoka init [--force] [OPTION]…

DESCRIPTION
       Initialise hisoka app

OPTIONS
       -f, --force
           force the initialisation
```
This command will initialize ```hisoka``` by creating an ```hisoka``` folder in your **$XDG_DATA_HOME** or **$HOME/.local/share** directory.
All the encrypted files will be stored here.


### - **Add**
To add a file, use the ```add``` subcommand
```
$ hisoka add --help
NAME
       hisoka-add - Add files to hisoka

SYNOPSIS
       hisoka add [--existing-group=EXISTING_GROUP] [--group=GROUP]
       [OPTION]… FILES…

DESCRIPTION
       Add files to hisoka

ARGUMENTS
       FILES (required)
           Files to add to hisoka

OPTIONS
       -e EXISTING_GROUP, --existing-group=EXISTING_GROUP
           Append the file to the group EXISTING_GROUP. The group must exist

       -g GROUP, --group=GROUP
           Append the file to the group GROUP
```

Each file can be added in one or several groups which allow to later filter by the group.

### - **Decrypt**
To restore encrypted files, use the ```decrypt``` subcommand
```
$ hisoka decrypt --help
NAME
       hisoka-decrypt - Decrypt encrypted files

SYNOPSIS
       hisoka decrypt [--group=GROUP] [--out-dir=DIRECTORY]
       [--strategy[=any|all|exact]] [OPTION]… [FILES]…

DESCRIPTION
       Decrypt encrypted files

ARGUMENTS
       FILES
           Files to decrypt. If no file is provided, all the files which are
           matched by the groups and strategy will be decrypted

OPTIONS
       -g GROUP, --group=GROUP
           Decrypt all files belonging to GROUP

       --out-dir=DIRECTORY (absent=current directory)
           Write decrypted files in the given DIRECTORY

       -s [any|all|exact], --strategy[=any|all|exact] (default=all)
       (required)
           the filter strategy to apply. "any" matches if at least one of the
           given groups belongs to the group of the file. "all" matches if
           all the given groups belongs to the group of the file. "exact"
           matches if exactly all the given groups are the same groups as the
           file
```

### - **Other**
There are other subcommands but those 3 explain pretty much how hisoka works.


## How to build
First you will need to install those opam packages.
```sh
$ opam install cryptokit dune xdg yojson cmdliner ppx_deriving_yojson
```

```sh
git clone https://github.com/EruEri/hisoka
cd hisoka
dune build
```



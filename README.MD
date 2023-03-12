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
This command will initialize ```hisoka``` by creating a ```.hisoka``` folder in your **HOME** directory.
All the encrypted files will be stored here.

#### **Warning**
The global varible ```$HOME``` must be set.

### - **Add**
To add a file, use the ```add``` subcommand
```
$ hisoka add --help
NAME
       hisoka-add - Add file to hisoka

SYNOPSIS
       hisoka add [--group=GROUP] [--mono] [--url=URL] [OPTION]… FILE

DESCRIPTION
       Add file to hisoka

ARGUMENTS
       FILE (required)
           file to encrypt

OPTIONS
       -g GROUP, --group=GROUP
           Append the file to a group of file

       -m, --monolithic, --mono
           whether the encrypted file should be merge within one big file
           containing others encrypted file

       --url=URL
           Url of the file. url option will download the file at the address
           --url and save it as FILE
```
Hisoka has two strategies to store files. The first one, is to store each file individually. The other one, _monolithic_, is to store each files in one big file.

Also, each file can be added in a group which allow to later filter by the group.

### - **Decrypt**
To restore encrypted files, use the ```decrypt``` subcommand
```
$ hisoka decrypt --help
NAME
       hisoka-decrypt - Decrypt encrypted files

SYNOPSIS
       hisoka decrypt [--group=GROUP] [--out-dir=DIRECTORY] [OPTION]… FILES…

DESCRIPTION
       Decrypt encrypted files

ARGUMENTS
       FILES (required)
           Decrypt all the files

OPTIONS
       -g GROUP, --group=GROUP
           Decrypt all files belonging to GROUP

       --out-dir=DIRECTORY (absent=current directory)
           Write decrypted files in the given DIRECTORY
```

### - **Display**
To directly show image files in your terminal, use the ```display``` subcommand

Use 'l' to switch to the next image, 'j' for the previous one and 'q' to quit. 
```
NAME
       hisoka-display - Display files in the terminals

SYNOPSIS
       hisoka display [--group=GROUP] [--mode[=Pixel Mode]] [OPTION]… [FILES]…

DESCRIPTION
       Display allows you to show files within hisoka directly into the
       terminal

ARGUMENTS
       FILES
           Files to display

OPTIONS
       -g GROUP, --group=GROUP
           Render files belonging to GROUP

       -m [Pixel Mode], --mode[=Pixel Mode] (default=none) (required)
           Specify the pixel mode to use to render the image. one of 'iterm',
           'kitty', 'sixel' or 'none'
```

### - **Other**
There are other subcommands but those 3 explain pretty much how hisoka works.


## How to build
- First you will need Ocaml >= 5.0.0 and install those opam packages.
    ```sh
    $ opam install cryptokit yojson cmdliner ppx_deriving_yojson
    ```

- You will also need to install those C libraries:
  - [Chafa](https://github.com/hpjansson/chafa)
  - [ImageMagick](https://github.com/imagemagick/imagemagick)
    - ImageMagick >= 7.0.0
    
  The C libraries must be able to be found by **pkg-config**

- And finally:
  ```sh
  git clone https://github.com/EruEri/hisoka
  cd hisoka
  dune build
  ```



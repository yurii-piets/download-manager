# Download Manager #

To start download manager run in **erlang** console: `app:main().`
    
After application launches.
Start download:
```
Enter command> start -l {url} -d {PATH_where_to_save_file}
```

Stop download:
```
Enter command> stop -l {url}
```

List of pending downloads:
```
Enter command> list
```

List of downloads that are waiting for download:
```
Enter command> queue
```  

Quit program (and terminate all pending downloads):
```
Enter command> quit
```
 
Example commands:

* ``start -l http://erlang.org/doc/apps/inets/inets.pdf -d C:/Users/plague/devnull/download/``
* ``start -l https://avatars3.githubusercontent.com/u/19465769?s=400&u=e36ae09f247b1eea121a793c83cb910625731fcb&v=4 -d C:/Users/plague/devnull/download/``
* ``start -l https://drive.google.com/uc?authuser=0&id=13TB78_qbHGB9h7J1uLTuju2dYP9r8biO&export=download -d C:/Users/plague/devnull/download/``
* ``start -l https://pp.userapi.com/c841033/v841033056/57cea/UWbash21jvs.jpg -d C:/Users/plague/devnull/download/``
* ``stop -l https://pp.userapi.com/c841033/v841033056/57cea/UWbash21jvs.jpg``
* ``list``
* ``queue``
* ``quit``

Basic principles:
1. For each download separate thread is created. Number of parallel downloads is limited to **8** and if the limit of parallel downloads is reached the next downloads will be added to the queue. When one download finishes, the next is pulled from the queue and is being executed.

2. Naming of downloaded file. Each operation of composing name of download file is executed one by one and stops if current step is executed successfully: 
  * response header is checked for presence of parameter `content-disposition` that contains name of downloaded file;
  * link is checked if it contains name of file (e.g. ``https://.../UWbash21jvs.jpg`` contains name of file);
  * Thirdly, last string after `/` is concatenated with file extension. File extension is parsed from `content-type` response header parameter, which is mapped to the file extensions(`convert.erl`). If header parameter is empty or current content type cannot be mapped to file extension, no extension is chosen.
  
  
  
    
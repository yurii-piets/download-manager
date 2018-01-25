# Download Manager

To start download manager run in **erlang** console: `app:main().`
    
After application launch.
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
``
Enter command> quit
``
 
Example commands:

* ``start -l http://erlang.org/doc/apps/inets/inets.pdf -d C:/Users/plague/devnull/download/``
* ``start -l https://avatars3.githubusercontent.com/u/19465769?s=400&u=e36ae09f247b1eea121a793c83cb910625731fcb&v=4 -d C:/Users/plague/devnull/download/``
* ``start -l https://drive.google.com/uc?authuser=0&id=13TB78_qbHGB9h7J1uLTuju2dYP9r8biO&export=download -d C:/Users/plague/devnull/download/``
* ``start -l https://pp.userapi.com/c841033/v841033056/57cea/UWbash21jvs.jpg -d C:/Users/plague/devnull/download/``
* ``stop -l https://pp.userapi.com/c841033/v841033056/57cea/UWbash21jvs.jpg``
* ``list``
* ``queue``
* ``quit``
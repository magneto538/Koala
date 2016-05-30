# What is Koala?
Koala is an advanced scripting library for Kontakt KSP developers.
The latest version is v1.0.
The Koala project is open source and covered by GNU GPL licensing system.

The official web page of the Koala project is available [here](http://davidemagni.com/?page_id=709).

# Pre-requisites
- [Sublime Text 3 BETA](https://www.sublimetext.com/3)
- [Koala](https://github.com/magneto538/Koala/archive/master.zip)
- [Kontakt 5 FULL](http://www.native-instruments.com/en/products/komplete/samplers/kontakt-5/)

# Quickstart
1. [Download](https://github.com/magneto538/Koala/archive/master.zip) this repository as a ZIP file.
2. **Extract** the ZIP file.
3. **Open Sublime Text 3**.
4. Go to **'Preferences' > 'Browse Packages...'**.
5. **Copy 'SublimeKSP' folder** inside the directory that opens by clicking on 'Browse Packages...'
6. Restart Sublime Text 3
7. **Copy 'Koala' folder** in the root directory of your KSP project.

# Import Koala into your project
• **Open your KSP project** in Sublime Text 3. At the very beginning of the file, add the following code:

```ksp
import "Koala/Koala.ksp"
```
• **Create the 'on init' callback** in your KSP project. At the very beginning of the callback, add the following code:
```ksp
Koala.init
```

# Congratulations!
Now you are ready to use Koala.

**WARNING**: it is strongly discouraged to use Koala on an already started project. If you want to get the maximum out of Koala, you should use it on a brand new project

# Downloads and contacts
- [Koala User's Manual](http://davidemagni.com/wp-content/uploads/2016/05/Koala-v1.0-User-Manual.pdf)
- [Contact Koala Support](mailto:koala@davidemagni.com)
- [Koala page](http://davidemagni.com/?page_id=709) on [davidemagni.com](http://davidemagni.com)

# GNU General Public License (GPL)
**Koala** is an open source project released under GNU General Public License (GPL).

Koala is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Koala is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Koala. If not, see http://www.gnu.org/licenses/.

Koala and all its content - included individual scripting files, the Manual and its content, all the functions and features - are protected by copyright and registered to Davide Magni.


  

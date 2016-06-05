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
- [Koala's web page](http://davidemagni.com/?page_id=709) on [davidemagni.com](http://davidemagni.com)

# MIT License

**Koala** is an open source project released under MIT License.

Copyright (c) 2016 Davide Magni

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

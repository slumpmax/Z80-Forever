# Z80-Forever
## Z80 Assembler/Disassembler with editor

### Features
* Open *.ASM for edit and assemble.
* Open *.BIN and auto disassemble it. <em><strong>(temporarily suspended)</strong></em>
* Open *.COM and auto disassemble it. <em><strong>(temporarily suspended)</strong></em>
* Added debugging features.

### Some keyboard shortcuts
* **CTRL+A**   *Select all text in current editor*
* **CTRL+F**   *Search text in current editor*
* **F3**   *Continue search text*
* **CTRL+G**   *Goto line number*
* **CTRL+Y**   *Delete current line*
* **CTRL+S**   *Save Assembly file*
* **CTRL+N**   *Create new Assembly document*
* **HOME**   *Move to the beginning of the line or the first letter of line
* Click on preview at debug tab to show/hide VRAM preview

### Some code support
* DB, .DB, DEFB, .DEFB and all other case are same as DB
* EQU/.EQU
* Label and Constant can use with or without ":"
* **#outext EXT** for change output file extention. (must exclude DOT, default is "bin")

### Not support now
* DB with string

### New feature but no document
* Include other files
* Structure data
* Etc

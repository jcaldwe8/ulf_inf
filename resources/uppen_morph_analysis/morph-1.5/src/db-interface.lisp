;;;
;;; Copyright 1992, 1993 The University of Pennsylvania
;;;
;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted, provided
;;; that the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting 
;;; documentation, and that the name of U. of Pennsylvania not be used in 
;;; advertising or publicity pertaining to distribution of the software  
;;; without specific, written prior permission.  U. of Pennsylvania makes no 
;;; representations about the suitability of this software for any purpose.  
;;; It is provided "as is" without express or implied warranty.
;;;
;;; THE UNIVERSITY OF PENNSYLVANIA DISCLAIMS ALL WARRANTIES WITH REGARD TO
;;; THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
;;; FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF PENNSYLVANIA BE LIABLE FOR
;;; ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;

;;;; ######################
;;;; ######################
;;;; WARNING: this file contains the beginnings of a port to CMU Common Lisp.
;;;;   This file should NOT be released.

(in-package "DB" :use '(LISP))

#+LUCID (use-package "LCL")
#+CMU (use-package "ALIEN")
#+CMU (use-package "C-CALL")

(export '(open-db db-get db-get-decoded close-db null-db chop))

;;;
;;; NOTE TO THE INSTALLER:
;;; Set this parameter to the system pathname of the top-level directory
;;; of this database package.
(defparameter *install-directory* "/mnt/linc/extra/xtag/morphdir/morph/")


(defparameter *db-dir* (concatenate 'string *install-directory* "db/"))
(defparameter *lib-dir* (concatenate 'string *install-directory* "lib/"))

;;;;
;;;;  Foreign C Functions and structures (defined in dbfuns.c)
;;;;


;;;   Name:
;;;       db - a foreign structure for db database files.
;;;   Slots:
;;;       close - pointer to function returning an int
;;;       sync - pointer to function returning an int
;;;       del - pointer to function returning an int
;;;       get - pointer to function returning an int
;;;       put.

#+LUCID (lcl::def-foreign-struct db
	                         (close :type (:pointer :signed-32bit))
	                         (sync :type (:pointer :signed-32bit))
				 (del :type (:pointer :signed-32bit))
				 (get :type (:pointer :signed-32bit))
				 (put :type (:pointer :signed-32bit))
				 (seq :type (:pointer :signed-32bit))
				 (type :type (:pointer :signed-32bit))
				 (openinfo :type (:pointer :signed-32bit)))

#+CMU (def-alien-type db (struct db
			         (close (* (function integer)))
			         (sync (* (function integer)))
				 (del (* (function integer)))
				 (get (* (function integer)))
				 (put (* (function integer)))
				 (seq (* (function integer)))
				 (type (* (function integer)))
				 (openinfo (* (function integer)))
				 ))

;;;   Name:
;;;       c-open-db
;;;   Args:
;;;       database-file - string
;;;   Return:
;;;       pointer to DB structure (see man page for db(3))
;;;   Description:
;;;       Returns a pointer to the DB structure returned by a
;;;       call to C_OPEN_DB (in dbfuns.c).  Opens the database
;;;       file <database-file>.  Returns nil if unsuccessful.

#+LUCID (lcl::def-foreign-function (c-open-db (:language :c)
					      (:name "_C_open_db")
					      (:return-type (:pointer db)))
	                              (database-file (:pointer :character))
	                              (dbp (:pointer db)))

#+CMU (def-alien-routine ("C_open_db" c-open-db) (* db)
                         (database-file c-string)
	                 (dbp (* db) :in-out))

;;;   Name:
;;;       c-get-db
;;;   Args:
;;;       word - string
;;;       database-file - string
;;;   Return:
;;;       string
;;;   Description:
;;;       Invokes C function C_GET_DB (in DBFUNS.C) to retrieve <word>
;;;       from the unencoded database <database-file>.  

#+LUCID (lcl::def-foreign-function (c-get-db (:language :c)
				             (:name "_C_db_get")
				             (:return-type :signed-32bit))
	                             (keystring (:pointer :character))
	                             (dbp (:pointer db))
				     (buffer (:pointer :character)))

#+CMU (def-alien-routine ("C_db_get" c-get-db) int 
	                 (keystring c-string)
	                 (dbp (* db))
			 (buffer c-string))

;;; Name fixup.
;#+CMU (defun c-get-db (key db buffer)
;	(alien-funcall (extern-alien "C_db_get" (function int c-string (* db) c-string))
;		       key db buffer))

;;;   Name:
;;;       c-get-decoded-db
;;;   Args:
;;;       word - string
;;;       database-file - string
;;;   Return:
;;;       string
;;;   Description:
;;;       Invokes C function C_GET_DECODED_DB to retrieve <word> 
;;;       from the encoded database <database-file>.  

#+LUCID (lcl::def-foreign-function (c-get-decoded-db (:language :c)
						     (:name "_C_db_get_decoded")
						     (:return-type :signed-32bit))
	                             (keystring (:pointer :character))
	                             (dbp (:pointer db))
				     (buffer (:pointer :character)))

#+CMU (def-alien-routine ("C_db_get_decoded" c-get-decoded-db) int
	                 (keystring c-string)
	                 (dbp (* db))
			 (buffer c-string))
;;; Name fixup.
;#+CMU (defun c-get-decoded-db (key db buffer)
;	(alien-funcall (extern-alien "C_db_get_decoded" (function int c-string (* db) c-string))
;		       key db buffer))


;;;   Name:
;;;       c-close-db
;;;   Args:
;;;       database - pointer to DB
;;;   Return:
;;;       int
;;;   Description:
;;;       invokes the C function C_CLOSE_DB (in dbfuns.c) to close
;;;       the database indicated by <database>

#+LUCID (lcl::def-foreign-function (c-close-db (:language :c)
					       (:name "_C_close_db")
					       (:return-type :signed-32bit))
	                              (database-file (:pointer DB)))

#+CMU (def-alien-routine ("C_close_db" c-close-db) int
	                 (database-file (* DB)))
	                 
;;;;
;;;; LISP functions
;;;;


;;;   Name:
;;;       open-db
;;;   Args:
;;;       file - <string>
;;;   Return:
;;;       foreign pointer of type (:pointer db)
;;;   Description:
;;;       Creates a foreign pointer to a DB structure, and then calls
;;;       C-OPEN-DB on the file named by <file>, passing the foreign
;;;       pointer by reference.  Returns the foreign pointer returned
;;;       by C-OPEN-DB.

(defun open-db (file)
  (check-type file string)
  (let ((foreign-str (malloc-foreign-string file))
	(for-ptr (malloc-foreign-pointer :type '(:pointer db))))
    (c-open-db foreign-str for-ptr)))

;;;   Name:
;;;       close-db
;;;   Args:
;;;       db-ptr - (:pointer DB)
;;;   Return:
;;;       NIL
;;;   Description:
;;;       Closes the database pointed to by <db-ptr> by calling C-CLOSE-DB.
;;;       Deallocates foreign pointer <db-ptr>.

(defun close-db (db-ptr)
  (if (eq -1 (c-close-db db-ptr))
      (format t "Error closing database.~%")
      (free-foreign db-ptr)))

;;;   Name:
;;;       null-db
;;;   Args:
;;;       db-ptr - (:pointer DB)
;;;   Return:
;;;       NIL/T
;;;   Description:
;;;       Test if the database pointed to by <db-ptr> is null
;;;       by calling C-NULL-DB.

(defun null-db (db-ptr)
  #+LUCID (zerop (lcl::foreign-pointer-address db-ptr))
  #+CMU   (zerop (system:sap-int (alien-sap db-ptr)))
  )

;;;  *ENTRY-BUFFER* is a buffer in foreign space which is passed by
;;;  GET-DB and GET-DECODED-DB as the space in which the C retr<ieval
;;;  functions can write the entry retrieved from the database.

(defparameter *entry-buffer*
  #+LUCID (lcl::make-foreign-pointer :type '(:pointer (:array :character (25000)))
				     :static t)
  #+CMU (make-alien (array char 25000))
  )

#+LUCID (setf (lcl::foreign-pointer-type *entry-buffer*) '(:pointer :character))

;;;   Name:
;;;       get-db
;;;   Args:
;;;       key - <string>
;;;       db - (:pointer DB)
;;;   Return:
;;;       NIL, <string>
;;;   Description:
;;;       Copies the lisp string <key> into "keystring", a foreign pointer
;;;       to a character string, which is then passed to C-GET-DB as
;;;       the key into the database pointed to by <db>.
;;;       The data retrieved from the database is written into
;;;       *ENTRY-BUFFER*.  Returns either nil (on an error or no data
;;;       found in <db>) or the string in *ENTRY-BUFFER* if <key> was
;;;       found successfully.

(defun db-get (key db)
  (declare (special *entry-buffer*))
  (let* ((keystring (malloc-foreign-string key))
	 (result (c-get-db keystring db *entry-buffer*)))
    (free-foreign keystring)
    (if (/= (abs result) 1)
	(foreign-string-value *entry-buffer*))))

;;;   Name:
;;;       get-decoded-db
;;;   Args:
;;;       key - <string>
;;;       db - (:pointer DB)
;;;   Return:
;;;       NIL, <string>
;;;   Description:
;;;       Copies the lisp string <key> into "keystring", a foreign pointer
;;;       to a character string, which is passed to C-GET-FROM-DB as
;;;       the key into the encoded database hashtable pointed to by <db>.
;;;       *ENTRY-BUFFER* is the buffer in which the data from the database
;;;       is written.  Returns either nil (on an error or no data found in
;;;       <db>) or the string in *ENTRY-BUFFER* if <key> was found
;;;       successfully.

(defun db-get-decoded (key db)
  (declare (special *entry-buffer*))
  (let* ((keystring (malloc-foreign-string key))
	 (result (c-get-decoded-db keystring db *entry-buffer*)))
    (free-foreign keystring)
    (if (/= (abs result) 1)
	(foreign-string-value *entry-buffer*))))

;;;;
;;;;  USEFUL TOOLS
;;;;

;;;    Name: malloc-foreign-string
;;;    Arg:  str - string
;;;    Return: Foreign pointer  (LUCID common lisp)
;;;    Description:  Allocates foreign memory for <str>, then copies the Lisp
;;;                  <str> into it, thereby passing it to foreign functions.
;;;          N.B.: Copied from Sun Common Lisp 4.0 Advanced User's Guide, p. 4-6

#+LCL4.0
(defun malloc-foreign-string (str)
  (check-type str string)
  (let ((f-str (lcl::malloc-foreign-pointer
		:type
		`(:pointer (:array :character (,(1+ (length str))))))))
    (setf (lcl::foreign-string-value f-str) str)
    (setf (lcl::foreign-pointer-type f-str) '(:pointer :character))
    f-str))

#+CMU
(defun malloc-foreign-string (str)
  (check-type str string)
  (let ((fstr (make-alien char (length str))))
    (setf fstr str)
    fstr))
  

;;;    Name:
;;;          chop
;;;    Arg:
;;;          <string> (from morphological database)
;;;    Returns:
;;;          list of three-element lists of strings
;;;    Description:
;;;          Input = "<root>\t<pos> {<info>}*{#<root>\t<pos> {<info>*}}*"
;;;          Iterates through the input, taking each entry (up to a "#") in turn,
;;;          chopping it into a three strings, and creating a list from them.
;;;          NB: If there's no <info>, the empty string is returned.
;;;          E.g:      "red\tA#red\tN sg#red\tV INF"
;;;                           ^  ^ ^  ^
;;;                  oldstart_|  | |  |_morphend
;;;                      rootend_| |_posend
;;;          ==> (("red" "A" "") ("red" "N" "sg") ("red" "V" "INF"))

(defun chop (str)
  (let (return-list rootend posend morphend oldstart)
    (loop with start = 0
	  with strlen = (length str)
	  initially (when (null str) (return nil))
	  do
	  (setq oldstart start)
	  (setq rootend (position #\Tab str :start oldstart))
	  (setq morphend (or (position #\# str :start (1+ rootend))
			     strlen))
	  (setq posend (or (position #\Space str :start (1+ rootend) :end morphend)
			   morphend))
	  (setq start (if (/= morphend strlen)
			  (1+ morphend)
			  oldstart))
	  collect (cut-string str oldstart rootend posend morphend)
	          into return-list
	  until (< start morphend)
	  finally (return return-list))))

(defun cut-string (str one two three four)
 "Returns a list of <str> chopped into three pieces, <one> being the index of
  the leftmost char, <four> the rightmost, and <two> and <three> the interior
  cut points. NB: Copies of the substrings are returned."
 ;;  E.g.: (cut-string "Gobble D. Gook" 3 6 9 12) => ("ble" "D." "Go")
 ;;  E.g.: (cut-string "Gobble D." 3 6 9 9) => ("ble" "D." "")
 (if (= three four)
     (list (subseq str one two)
	   (subseq str (1+ two) three)
	   (subseq str three four))
     (list (subseq str one two)
	   (subseq str (1+ two) three)
	   (subseq str (1+ three) four))))

;;; Dependent code:

(defun free-foreign (ptr)
  #+LUCID (lcl::free-foreign-pointer ptr)
  #+CMU   (free-alien ptr))

(defun malloc-foreign-pointer (&key type)
  #+LUCID (lcl::malloc-foreign-pointer :type '(:pointer db))
  #+CMU   (make-alien (* db)))

(defun foreign-string-value (for-str)
  #+LUCID (lcl::foreign-string-value for-str)
  #+CMU   for-str)

;;; At the very last, load the foreign libraries

(defun load-foreign-libraries (lib-list)
  #+LUCID (lcl::load-foreign-libraries nil lib-list)
  #+CMU   (load-foreign "" :libraries lib-list))
	   
;(load-foreign-libraries (list (concatenate 'string *lib-dir* "libhash.a")
;			      (concatenate 'string *lib-dir* "libmorph.a")
;			      "-lc"))

;(alien:load-foreign "" :libraries '("-lhash" "-lmorph" "-lc" "-L/mnt/linc/extra/xtag/morphdir/morph/lib"))



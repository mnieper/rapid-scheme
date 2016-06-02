;;; Rapid Scheme --- An implementation of R7RS

;; Copyright (C) 2016 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (format format-string . objects)
  (let ((buffer (open-output-string)))
    (let loop ((format-list (string->list format-string))
               (objects objects))
      (cond
       ((null? format-list) (get-output-string buffer))
       ((char=? (car format-list) #\~)
        (if (null? (cdr format-list))
            (error "format: incomplete escape sequence" format-string)
            (case (cadr format-list)
              ((#\a)
               (cond
                ((null? objects)
                 (error "format: no value for escape sequence ‘~a’" format-string))
                (else
                 (display (car objects) buffer)
                 (loop (cddr format-list) (cdr objects)))))
              ((#\s)
               (cond
                ((null? objects)
                 (error "format: no value for escape sequence ‘~s’" format-string))
                (else
                 (write (car objects) buffer)
                 (loop (cddr format-list) (cdr objects)))))
              ((#\%)
               (newline buffer)
               (loop (cddr format-list) objects))
              ((#\~)
               (write-char #\~ buffer)
               (loop (cddr format-list) objects))
              (else
               (error (format "format: unrecognized escape sequence ‘~~~a’"
                              (cadr format-list)) format-string)))))
       (else
        (write-char (car format-list) buffer)
        (loop (cdr format-list) objects))))))

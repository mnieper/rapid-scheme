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

(define (parallel-move* sources targets)
  (let*
      ((target-map
	(let loop ((map (imap equal?)) (sources sources) (targets targets))
	  (if (or (null? sources) (null? targets))
	      map
	      (loop (imap-replace map
				  (car sources)
				  (iset-adjoin (imap-ref/default map (car sources) (iset eq?))
					       (car targets)))
		    (cdr sources)
		    (cdr targets)))))
       (source?
	(lambda (exp) 
	  (and (imap-ref/default target-map exp #f) #t)))
       (graph
	(let loop ((graph '()) (entries (imap->alist target-map)))
	  (if (null? entries)
	      graph
	      (loop (cons (cons (caar entries)
				(iset->list (iset-filter source? (cdar entries))))
			  graph)
		    (cdr entries))))))
    (let loop ((scc (reverse (graph-scc graph equal?))))
      (if (null? scc)
	  '()
	  (let* ((sources (car scc))
		 (independent?
		  (lambda (target)
		    (not (memq target sources))))
		 (sources
		  (sort sources
			(lambda (s1 s2)
			  (or (eq? s1 (car sources))
			      (and (not (eq? s2 (car sources)))
				   (iset-member? (imap-ref target-map s1) s2)))))))
	    `(,@(apply append
		       (map (lambda (source)
			      (map (lambda (target)
				     `(move ,source ,target))
				   `,(iset->list (iset-filter independent?
							      (imap-ref target-map source)))))
			    sources))
	      ,@(if (null? (cdr sources))
		    (loop (cdr scc))
		    `((xchg ,@sources) . ,(loop (cdr scc))))))))))

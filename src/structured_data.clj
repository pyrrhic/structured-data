(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [one (get v 0)
        three (get v 2)]
    (+ one three)))
  
(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px1 py1] point]
    (and (<= x1 px1 x2)
         (<= y1 py1 y2))))

(defn contains-rectangle? [outer inner]
  (let [
        bottom-left (get inner 0)
        top-right (get inner 1)]
    (and
      (contains-point? outer bottom-left)
      (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [existing-authors (:authors book)
        combined-authors (conj existing-authors new-author)]
  (assoc book :authors combined-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [descending (apply <= a-seq)
        ascending (apply >= a-seq)]
    (or descending ascending)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [size-after (count (set a-seq))
        size-before (count a-seq)]
    (not (= size-after size-before))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [all-authors (:authors book)]
    (contains? all-authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        years (str " (" birth-year " - " death-year ")")]
    (if (nil? birth-year) 
      author-name
      (str author-name years))))

(defn authors->string [authors]
  (apply str 
         (interpose ", " 
                    (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        filler ", written by "
        authors (authors->string (:authors book))]
    (str title filler authors)))

(defn books->string [books]
  (let [num-books (count books)
        num-books-formatted (cond 
                              (= num-books 0) "No books"
                              (> num-books 1) (str num-books " books") 
                              :else (str num-books " book"))
        no-periods (cons num-books-formatted (map book->string books))]
    (str (apply str (interpose ". " no-periods)) ".")))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [a authors
        author-map (filter (fn [x] (if (= name (:name x)) true false)) a)]
    (first author-map)))
  

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (let [all-authors (:authors book)]
    (not (empty? (living-authors all-authors)))))
    

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%

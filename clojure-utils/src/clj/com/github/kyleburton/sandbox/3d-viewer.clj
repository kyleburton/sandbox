;; taken from the files section of the Clojure google group:
;; http://groups.google.com/group/clojure/files

;; - dragging (holding down the button and moving) the mouse rotates the coordinate space
;; - moving the mouse wheel zooms in/out

(import '(java.awt Canvas Color)
        '(javax.swing JFrame)
	'(java.awt.event MouseAdapter MouseEvent MouseMotionListener MouseWheelListener))

(defn projectX [radius theta phi width x y z]
  "Returns the x pixel"
  (int (+ (/ width 2)
	  (* radius (+ (* x (Math/cos phi))
		       (* y (Math/sin phi)))))))

(defn projectY [radius theta phi height x y z]
  "Returns the y pixel"
  (int (- (/ height 2)
	  (* radius (+ (* (- x) (Math/sin phi) (Math/cos theta))
		       (* y (Math/cos phi) (Math/cos theta))
		       (* z (Math/sin theta)))))))

(defn g3d [coords]
  "Opens a 3d viewer in a new JFrame.  coords is a seq of 3-tuples"
  (let [xI (ref 0) yI (ref 0)
	left (ref false) right (ref false)
	radius (ref 10)
	theta (ref (/ Math/PI 2))
	phi (ref (/ Math/PI 2))
	jframe (JFrame.)
	plot-coords (fn [g]
		      (. g setColor Color/white)
		      (. g fillRect 0 0 (. jframe getWidth) (. jframe getHeight))
		      (. g setColor Color/black)
		      (dorun (for [coord coords]
			       (. g fillOval
				  (apply projectX @radius @theta @phi (. jframe getWidth) coord)
				  (apply projectY @radius @theta @phi (. jframe getHeight) coord)
				  2 2))))
	canvas (proxy [Canvas] []
		 (paint [graphics] (plot-coords graphics)))
	mouse (proxy [MouseAdapter] []
		(mousePressed [e] (dosync (ref-set xI (. e getX))
					  (ref-set yI (. e getY))
					  (if (= (. e getButton) MouseEvent/BUTTON1)
					    (ref-set left true))
					  (if (= (. e getButton) MouseEvent/BUTTON2)
					    (ref-set right true))))
		(mouseReleased [e] (dosync (if (= (. e getButton) MouseEvent/BUTTON1)
					     (ref-set left false))
					   (if (= (. e getButton) MouseEvent/BUTTON2)
					     (ref-set right false)))))
	motion (proxy [MouseMotionListener] []
		 (mouseDragged [e] (let [dx (- @xI (. e getX))
					 dy (- @yI (. e getY))]
				     ;(if @right;translate and repaint
				     ;  )
				     (if @left
				       (dosync (alter phi + (/ (* dx Math/PI) (. jframe getWidth)))
					       (alter theta + (/ (* dy Math/PI) (. jframe getHeight)))
					       (ref-set xI (. e getX))
					       (ref-set yI (. e getY))))
				     (.repaint canvas)))
		 (mouseMoved [e]))
	wheel (proxy [MouseWheelListener] []
		(mouseWheelMoved [e] (let [clicks (. e getWheelRotation)
					   percent 1.10]
				       (dosync (if (< 0 clicks)
						 (alter radius / (* (Math/abs clicks) percent))
						 (alter radius * (* (Math/abs clicks) percent))))
				       (.repaint canvas))))
	]
    (doto canvas
      (.addMouseListener mouse)
      (.addMouseMotionListener motion)
      (.addMouseWheelListener wheel))
    (doto jframe
      (.add canvas)
      (.setSize 640 480)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.pack)
      (.setVisible true))))

(def *box*
     [
      [0 0 0]
      [0 0 1]
      [0 0 2]
      [0 0 3]
      [0 0 4]
      [0 0 5]

      [0 5 0]
      [0 5 1]
      [0 5 2]
      [0 5 3]
      [0 5 4]
      [0 5 5]
          
      [5 0 0]
      [5 0 1]
      [5 0 2]
      [5 0 3]
      [5 0 4]
      [5 0 5]
          
      [5 5 0]
      [5 5 1]
      [5 5 2]
      [5 5 3]
      [5 5 4]
      [5 5 5]

      [0 0 0]
      [0 1 0]
      [0 2 0]
      [0 3 0]
      [0 4 0]
      [0 5 0]

      [0 0 5]
      [0 1 5]
      [0 2 5]
      [0 3 5]
      [0 4 5]
      [0 5 5]

      [5 0 0]
      [5 1 0]
      [5 2 0]
      [5 3 0]
      [5 4 0]
      [5 5 0]

      [5 0 5]
      [5 1 5]
      [5 2 5]
      [5 3 5]
      [5 4 5]
      [5 5 5]

      [0 0 0]
      [1 0 0]
      [2 0 0]
      [3 0 0]
      [4 0 0]
      [5 0 0]

      [0 0 5]
      [1 0 5]
      [2 0 5]
      [3 0 5]
      [4 0 5]
      [5 0 5]
            
      [0 5 0]
      [1 5 0]
      [2 5 0]
      [3 5 0]
      [4 5 0]
      [5 5 0]
            
      [0 5 5]
      [1 5 5]
      [2 5 5]
      [3 5 5]
      [4 5 5]
      [5 5 5]

      ])

(g3d *box*)

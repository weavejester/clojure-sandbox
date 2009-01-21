(ns clojure.utils.ppxml
  (:import javax.xml.parsers.DocumentBuilderFactory
           javax.xml.transform.TransformerFactory
           javax.xml.transform.dom.DOMSource
           javax.xml.transform.stream.StreamResult
           org.xml.sax.InputSource))

(defn- xml->dom
  "Turn an reader of an XML source into a DOM."
  [reader]
  (.. (DocumentBuilderFactory/newInstance)
      (newDocumentBuilder)
      (parse (InputSource. reader))))

(defn- doc-element
  "Get the normalized document element from a dom."
  [dom]
  (doto (.getDocumentElement dom)
        (.normalize)))

(defn- new-transformer
  "Create a new Transformer for serializing."
  []
  (.newTransformer
    (doto (TransformerFactory/newInstance)
          (.setAttribute "indent-number" 2))))

(defn- set-output-properties
  "Set the output properties for a transformer."
  [transformer properties]
  (doseq [[key val] properties]
    (.setOutputProperty transformer key val))
  transformer)

(defn- dom->xml
  "Turn a DOM into nicely formatted XML."
  [dom writer]
  (let [document   (doc-element dom)
        serializer (new-transformer)]
    (set-output-properties serializer
      {"method" "xml"
       "omit-xml-declaration" "yes"
       "indent" "yes"})
    (.transform serializer
       (DOMSource. document)
       (StreamResult. writer))))

(defn ppxml
  "Pretty print a string of XML."
  [xml]
  (with-in-str xml
    (-> (xml->dom *in*) (dom->xml *out*))))

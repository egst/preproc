<html>
    <body>
        <h1>|=data["title"]|</h1>
        <section>
|           for article in data["articles"]
                <article>
                    <h2>|=article["title"]|</h2>
|                   for content in article["contents"]
|                       let tagName = content["tag"];
                            val = content["val"]
|                           let oTag = "<"++tagName++">";
                                cTag = "</"++tagName++">"
|                               if tagName == "img"
                                    <img src="|=val|">
|                               else
                                    |=oTag++val++cTag|
|                               end
|                           end
|                       end
|                   end
                </article>
|           end
        </section>
    </body>
</html>
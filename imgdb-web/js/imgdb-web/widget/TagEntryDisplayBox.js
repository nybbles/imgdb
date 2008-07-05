dojo.provide("imgdb-web.widget.TagEntryDisplayBox");
dojo.require("dijit.InlineEditBox");
dojo.require("dijit.form.TextBox");
dojo.require("dijit._Templated");

dojo.declare
(
 "imgdb-web.widget.TagEntryDisplayBox",
 [dijit._Widget, dijit._Templated],
 {
     entrytext: "Add tags",
     templatePath: dojo.moduleUrl("imgdb-web.widget",
                                  "templates/TagEntryDisplayBox.htm"),
     templateString: "",
     widgetsInTemplate: true,
     initialtagset: ["car", "audi", "flower", "macro"],
     tagset: [],
     postCreate: function()
     {
       this.tagentrybox.displayNode.style.color = "#777777";

       dojo.forEach
         (this.initialtagset,
          function(tag)
          {
            this._addTagDOMNode(tag);
          },
          this);
     },
     _addTagsEventHandler: function(value)
     {
       var taglist = value.split(/\s+/).reverse();
       this._addTags(taglist);
     },
     _addTags: function(taglist)
     {
       var tagdata = {imgid : taglist};
       var widget = this;
       dojo.rawXhrPost({
             url: "/add-img-tags",
             handleAs: "json-comment-filtered",
             postData: dojo.toJson(tagdata),
             timeout: 1000,
             load: function(response, ioargs)
             {
               // This code does not handle the case where tags have
               // since been deleted.
               dojo.forEach
                 // (response.imgid,
                 (response.tags,
                  function(tag)
                  {
                    widget._addTagDOMNode(tag);
                  },
                  this);
               
               widget.tagentrybox.setValue("");
             },
             error: function(response, ioargs)
             {
               alert(ioargs.xhr.status + ": " + response);
             }
       });
     },
     _addTagDOMNode: function(tag)
     {
       if (!this._doesTagExist(tag))
       {
         var newTagDiv = document.createElement("div");
         newTagDiv.className = "tag";
         newTagDiv.tag = tag;

         var newTag = document.createElement("a");
         newTag.className = "tagname";
         newTag.innerHTML = tag;

         var newTagDelete = document.createElement("a");
         newTagDelete.className = "tagdelete";
         newTagDelete.innerHTML = "[x]";
         newTagDelete.tag = tag;
         dojo.connect(newTagDelete, 'onclick', this, "_deleteTagEventHandler");
        
         newTagDiv.appendChild(newTag);
         newTagDiv.appendChild(newTagDelete);

         this.taglist.insertBefore(newTagDiv, this.taglist.firstChild);
         this.tagset[this.tagset.length] = tag;
       }
     },
     _deleteTagEventHandler: function(event)
     {
       this._deleteTag(event.target.tag);
     },
     _deleteTag: function(tag)
     {
       var i = 0;
       var tagFound = 0;

       for (i = 0; i < this.tagset.length; ++i)
       {
         if (tag === this.tagset[i])
         {
           tagFound = 1;
           break;
         }
       }

       if (tagFound)
       {
         this.tagset.splice(i, 1);
       }

       for (i = 0; i < this.taglist.childNodes.length; ++i)
       {
         if (this.taglist.childNodes[i].tag === tag)
         {
           this.taglist.removeChild(this.taglist.childNodes[i]);
         }
       }
     },
     _doesTagExist: function(tag)
     {
       for (i = 0; i < this.tagset.length; ++i)
       {
         if (tag === this.tagset[i])
         {
           return 1;
         }
       }

       return 0;
     }
 }
);

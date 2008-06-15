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
     tagset: ["car", "audi", "flower", "macro"],
     postCreate: function()
     {
       this.tagentrybox.displayNode.style.color = "#777777";

       dojo.forEach
         (this.tagset,
          function(tag)
          {
            this._addTag(tag);
          },
          this);
     },
     _updateTagList: function(value)
     {
       var tagList = value.split(/\s+/);
       
       dojo.forEach
         (tagList.reverse(),
          function(tag)
          {
            this._addTag(tag);
          },
          this);
       
       this.tagentrybox.setValue("");
     },
     _addTag: function(tag)
     {
       var newTag = document.createElement("li");
       newTag.innerHTML = tag;
       this.taglist.insertBefore(newTag, this.taglist.firstChild);
     }
 }
);

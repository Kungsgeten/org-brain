#+TITLE:org-brain [[http://melpa.org/#/org-brain][file:http://melpa.org/packages/org-brain-badge.svg]]

=org-brain= implements a variant of [[https://en.wikipedia.org/wiki/Concept_map][concept mapping]] in Emacs, using [[http://orgmode.org/][org-mode]]. It
is heavily inspired by a piece of software called [[http://thebrain.com/][The Brain]], and you can view an
introduction to that program [[https://www.youtube.com/watch?v=GFqLUBKCFdA][here]]. They also provide [[https://www.thebrain.com/blog/][a blog]] with great ideas of
how you can think when organizing your Brain.

You can think of =org-brain= as a combination of a wiki and a mind map, where
each wiki page / mind map node is an =org-mode= file which resides in your
=org-brain-path=, or a headline with an ID property in one of those files. These
are called /entries/. Entries can be linked together, and you can then
view the network of links as a mind map, using =M-x org-brain-visualize=. Here's [[https://www.youtube.com/watch?v=3EGOwfWok5s&t=][a video introducing =org-brain=]].

#+BEGIN_EXAMPLE
  PINNED:  Index


                 +-Python              Game development-+-Game design
                 +-Programming books           |
     Programming-+-Emacs                       |
           |                                   |
           +-----------------+-----------------+
                             |
                             V
                      Game programming <-> Computer games

  Game Maker  Unity

  --- Resources ---------------------------------

  - https://en.wikipedia.org/wiki/Game_programming
  - Passing Through Ghosts in Pac-Man
  - In-House Engine Development: Technical Tips

  --- Text --------------------------------------

  Game programming is the art of programming computer games...
#+END_EXAMPLE

When visualizing an entry, you will see the entry's relationship to other
entries. There are four different types of relationships in =org-brain=:

- Parents :: Entries above the visualized entry. If the visualized entry is a
             headline, then the parent headline in the =org-mode= file will be
             one of the parents. In the case of top level headlines, the file
             itself will be considered a parent. Additional parents can be added
             manually. In the example above, /Programming/ and /Game
             development/ are parents of the visualized /Game programming/
             entry.
- Children :: Entries below the visualized entry. This will by default be
              subheadings of the visualized entry (or level one headlines, if
              the visualized entry is a file). You can add other children,
              residing elsewhere, manually. In the example above, /Game Maker/
              and /Unity/ are the children of /Game programming/.
- Siblings :: These appear to the right of the parent entries. Siblings are the
              other children of the visualized entry's parents.
- Friends :: These appear to the right of the visualized entry. Friends provide
             a way of adding a hierarchy independent two-way relationship
             between two entries. Friends must be added manually. In the example
             above, /Computer games/ and /Game programming/ are friends.

[[http://blogarchive.thebrain.com/thought-relationships/][Here's an article]] describing how you can use the different relationships (The
Brain's /jump thoughts/ are the equivalent of /friends/ in =org-brain=).

Apart from the visualized entry's relationships, =org-brain-visualize= also show
pinned entries, which are shown independently of the visualized entry; /Index/
is a pinned entry in the example above. =org-brain-visualize= also show a list
of the entry's resources (links and attachments), and the text in the entry. The
example above have three resources, and a short text. The resources and text is
gathered from =org-mode= automatically.

There's also the option to visualize the entry as a tree, or similar to a
mind map, where you can zoom in order to show grandparents and grandchildren.

The relationship entries, pinned entries and resources are all links; they can
be pressed/clicked to visualize other entries, visit resources etc.

You can also annotate the connection between the visualized entry and one of the
other entries. You can think of it as annotating the edge between two nodes in a
graph. Annotations will show up in the mini-buffer when hovering over an
annotated connection.

* Setup and requirements

The easiest way is to get =org-brain= from MELPA. If you do not want to do that,
clone this git repository or download =org-brain.el= and add it to your load-path.
The example below is using [[https://github.com/jwiegley/use-package][use-package]] and assumes that you're using MELPA, but
you could use =(require 'org-brain)= or add a =:load-path= to =use-package= instead.
Most of the configuration below isn't necessary, but showcases some options.

#+BEGIN_SRC emacs-lisp
  (use-package org-brain :ensure t
    :init
    (setq org-brain-path "directory/path/where-i-want-org-brain")
    ;; For Evil users
    (with-eval-after-load 'evil
      (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
    :config
    (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
    (setq org-id-track-globally t)
    (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
    (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?" :empty-lines 1)
          org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 12)
    (setq org-brain-include-file-entries nil
          org-brain-file-entries-use-title nil))

  ;; Allows you to edit entries directly from org-brain-visualize
  (use-package polymode
    :config
    (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))
#+END_SRC

1. =org-brain= requires Emacs 25 and org-mode 9. These need to be part of your
   Emacs.
2. Configure =org-brain-path= (defaults to =/brain= in your =org-directory=) to
   a directory where you want to put your =org-brain= files (which could be the
   location where you already keep your org files if you wish to transform your
   existing org files into =org-brain= files). You can set this with the example
   config presented above or through =M-x customize-group RET org-brain=.
3. If you're an [[https://github.com/emacs-evil/evil][evil]] user, you'll want to add =(evil-set-initial-state
   'org-brain-visualize-mode 'emacs)= to your =org-brain= configuration.
4. =org-brain= use =org-id= in order to speed things up. Because of this, the
   variable =org-id-track-globally= should be =t= (which it already is by default).
   You may want to modify =org-id-locations-file= too. If you add entries to
   =org-brain= directly from =org-mode= you must assign headlines an ID. A
   comfortable way to do this is with the command
   =org-brain-ensure-ids-in-buffer=. Even more comfortable is to add that to
   =before-save-hook=, so that it runs when saving.
5. =org-brain-prefix-map= can be bound to a key to make =org-brain= commands more
   accessable if you edit entries from =org-mode=. See /Editing from org-mode/ under
   /Usage/ below.
6. You might want to add information at the end of an entry, without visiting
   the file. A way to do this is to use a [[http://orgmode.org/manual/Capture.html][capture]] template, such as the one
   presented above.
7. If you have a lot of entries, it might take some time to gather information
   about all entries when using =org-brain-visualize=. You could change the
   value of =org-brain-visualize-default-choices= (which is ='all= by default)
   to only include files, or even just files in the direct root of
   =org-brain-path=.
8. If you feel that =org-brain-visualize= is too cluttered, you may want to set
   =org-brain-show-resources= and/or =org-brain-show-text= to =nil=.
9. If you have very long entry names, =org-brain-visualize= may take a lot of
   horizontal space. You can cap the shown length of entry titles, by setting
   =org-brain-title-max-length=.
10. Some users find it confusing having both /headline/ entries and /file/ entries
    (see below). It may be preferable to only use headline entries, by setting
    =org-brain-include-file-entries= to =nil=. If doing this, you should probably
    also set =org-brain-file-entries-use-title= to =nil=. Another possibility is if
    you're only using file entries, in which case you can set
    =org-brain-scan-for-header-entries= to =nil=.
11. =polymode= is a package (available on MELPA) which allows for several
    major-modes in the same buffer. If you have required the package you can use
    =M-x org-brain-polymode= inside =org-brain-visualize=, or (as in the example
    above) add =org-brain-polymode= to =org-brain-visualize-mode-hook=.

** Category icons

=org-brain= supports showing icons for your entries, depending on their [[https://orgmode.org/manual/Categories.html][category]]. Use the variable =org-agenda-category-icon-alist= to specify icons for categories.

See example of using /all-the-icons/ for this below under /Other useful packages/.

** Customizing the look of entry titles

When visualizing you might want to see additional information about the entries. This can be done by customizing the following variables:

- =org-brain-vis-title-prepend-functions=
- =org-brain-vis-title-append-functions=
- =org-brain-vis-current-title-prepend-functions=
- =org-brain-vis-current-title-append-functions=

Each of these variables should be a list of functions, which all takes an entry as the single parameter and returns a string. This string is the prepended or appended to the entry's title, according to the name of the function. The variables with the name =current= in them only applies the functions on the currently visualized entry (the focused one).

Suitable functions to add to these lists might be:

- =org-brain-entry-icon= (included in =org-brain-vis-title-prepend-functions= by default)
- =org-brain-entry-todo-state=
- =org-brain-entry-tags-string=

* Headline and file entries

There are two types of entries in =org-brain=: /headline/ entries and /file/
entries. For the most part these are used the same way, and the main difference
between them is how their content is stored inside your =org-brain= directory.
All .org-files inside the =org-brain-path= are considered as /file/ entries (the
content typically being the text before the first headline in the file) and all
headlines /with an ID property/ inside these files are considered as /headline/
entries.

By default subdirectories inside =org-brain-path= are scanned recursively for
files, so all subdirectories and their files are considered part of the brain.
You can choose to only have the root of =org-brain-path= be scanned for files, by
setting =org-brain-scan-directories-recursively= to =nil=.

If you have a headline entry, which you want to convert to a file entry, use =M-x
org-brain-headline-to-file=. Unfortunately there is currently no function to
convert a file entry into a headline entry.

** Limiting =org-brain= to headline entries

Most of =org-mode= is tailored towards working with headlines, and because of this
=org-brain= has some limitations regarding what's possible with /file entries/. The
concept of both headline and file entries is confusing to some users.

If you prefer to only use headline entries, you can set the variable
=org-brain-include-file-entries= to =nil=. It then also makes sense to set
=org-brain-file-entries-use-title= to =nil=.

You may choose to exclude the file part of entry names when choosing among
entries. =org-brain= passes two objects, the file and the headline, to the emacs
[[https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html][format]] function. By setting =org-brain-headline-entry-name-format-string= to
="%2$s"=, =org-brain= will only show the headline title.

Here's an example which may be suitable for a setup without file entries:

#+BEGIN_SRC emacs-lisp
  (setq org-brain-include-file-entries nil)
  (setq org-brain-file-entries-use-title nil)
  (setq org-brain-headline-entry-name-format-string "%2$s")

  (setq my/default-org-brain-file "brain")
  (setq org-brain-default-file-parent my/default-org-brain-file)
#+END_SRC

** Limiting =org-brain= to file entries

If you instead prefer to work with file entries, you can set
=org-brain-scan-for-header-entries= to =nil=. It will still be possible to add
headline entries, but they will be excluded from most searches when choosing
among entries.

* Usage

You can create new entries by =M-x org-brain-visualize= and then
typing the name of a non-existing entry. You can go back to the
=*org-brain*= buffer by =M-x org-brain-visualize-dwim=. It will switch
to the =*org-brain*= buffer. If there's no such buffer, or if already
there, run =org-brain-visualize=. You could also use =M-x
org-brain-add-entry= if you do not want to visualize the new entry.
Also commands which add children, parents and friends, or links to
entries, can create new entries in the same way. Se /General
information/ below.

If you find that =org-brain= is missing entries, or list entries which doesn't
exist, try using =M-x org-brain-update-id-locations=, which syncs the
=org-brain= entries with the =org-id= caching system.

** =org-brain-visualize=

The primary usage of =org-brain= is through =M-x org-brain-visualize= (which you
might want to bind to a key). From there you can browse entries, add/remove
relationships, open entries for editing etc. The following keybindings are
available in =org-brain-visualize=:

| Key         | Command                            | Description                                                                       |
|-------------+------------------------------------+-----------------------------------------------------------------------------------|
| m           | =org-brain-visualize-mind-map=       | Toggle between normal and mind-map visualization.                                 |
| j or TAB    | =forward-button=                     | Goto next link                                                                    |
| k or S-TAB  | =backward-button=                    | Goto previous link                                                                |
| b           | =org-brain-visualize-back=           | Like the back button in a web browser.                                            |
| h or *      | =org-brain-add-child-headline= *     | Add a new child /headline/ to entry                                                 |
| c           | =org-brain-add-child= *              | Add an existing entry, or a new /file/, as a child                                  |
| C           | =org-brain-remove-child= *           | Remove one the entry's child relations                                            |
| e           | =org-brain-annotate-edge=            | Annotate the connection between the visualized entry and the entry link at point. |
| p           | =org-brain-add-parent= *             | Add an existing entry, or a new /file/, as a parent                                 |
| P           | =org-brain-remove-parent= *          | Remove one of the entry's parent relations                                        |
| f           | =org-brain-add-friendship= *         | Add an existing entry, or a new /file/, as a friend                                 |
| F           | =org-brain-remove-friendship= *      | Remove one of the entry's friend relations                                        |
| n           | =org-brain-pin= *                    | Toggle if the entry is pinned or not                                              |
| N           | =org-brain-add-nickname=             | Add a nickname for the entry (you can have several names for the same entry)      |
| s           | =org-brain-select-dwim=              | Select an entry for batch processing.                                             |
| S           | =org-brain-select-map=               | Prefix key to do batch processing with selected entries.                          |
| t           | =org-brain-set-title=                | Change the title of the entry.                                                    |
| T           | =org-brain-set-tags=                 | Change the tags of the entry.                                                     |
| d           | =org-brain-delete-entry=             | Choose an entry to delete.                                                        |
| l           | =org-brain-visualize-add-resource=   | Add a new resource link in entry                                                  |
| r           | =org-brain-open-resource=            | Choose and open a resource from the entry.                                        |
| C-y         | =org-brain-visualize-paste-resource= | Add a new resource link from clipboard                                            |
| a           | =org-brain-visualize-attach=         | Run =org-attach= on entry (headline entries only)                                   |
| A           | =org-brain-archive=                  | Archive the entry (headline entries only)                                         |
| o           | =org-brain-goto-current=             | Open current entry for editing                                                    |
| O           | =org-brain-goto=                     | Choose and edit one of your =org-brain= entries                                     |
| v           | =org-brain-visualize=                | Choose and visualize a different entry                                            |
| V           | =org-brain-visualize-follow=         | Similar to =org-agenda-follow-mode=; view visualized entry in another window.       |
| w           | =org-brain-visualize-random=         | Visualize one of your entries at random.                                          |
| W           | =org-brain-visualize-wander=         | Visualize at random, in a set interval. =W= again to cancel.                        |
| C-c C-x C-v | =org-toggle-inline-images=           | Display org-mode images in the entry text.                                        |
| M           | Move prefix                        | Move (refile) the current entry.                                                  |
| M r         | =org-brain-refile=                   | Move current entry to another entry (change local parent).                        |
| M p         | =org-brain-change-local-parent=      | Choose among the entry's parents and make another of them the local parent.       |

The commands above marked with =*= can be run with the universal argument =C-u= in
order to operate on the entry link at point instead of the visualized entry.

You may use =org-store-link= inside of =org-brain-visualize= in order to store a
link to the currently visualized =org-brain= entry.

If the /universal argument/ =C-u= is used when running =org-brain-visualize-random= or
=org-brain-visualize-wander=, the randomized targets are restricted to descendants
(children, grandchildren, grand-grandchildren etc) of the currently visualized
entry. Use for instance =C-u W= to wander among the descendants.

The /universal argument/ =C-u= can also be used with =org-brain-open-resource=. This
lets you choose not only resource from the visualized entry, but also from
descendants (children, grand-children, etc) of that entry.

If the /universal argument/ =C-u= is used when calling =org-brain-annotate-edge= then
the annotation will be "one-way". The default behaviour is otherwise to annotate
the connection in both directions.

When using the mind map visualization (toggle by pressing =m=), you can use the
following keybindings in order to show or hide successive levels of hierarchy
relative to the current entry.

| Key | Command                         | Description                                                              |
|-----+---------------------------------+--------------------------------------------------------------------------|
| +   | =org-brain-show-descendant-level= | Show one more level of entries to the right (children of children, etc.) |
| -   | =org-brain-hide-descendant-level= | Hide rightmost level of descendant entries                               |
| z   | =org-brain-show-ancestor-level=   | Show one more level of entries to the left (parents of parents, etc.)    |
| Z   | =org-brain-hide-ancestor-level=   | Hide leftmost level of ancestor entries                                  |

If you want to select several entries and then remove/add them as
children/parents/friends you can use the =s= key (=org-brain-select-dwim=) to select
an entry. If the point is over a button link to an entry, that entry will be
selected, otherwise it will select the currently visualized entry. If the entry
is already selected, it will be unselected instead.

Once you have selected all the entries you wish to use, you can use the =S= prefix
key to do batch processing on the selected entries. The keybindings in this
prefix keymap is identical to those in =org-brain-visualize=. You could for
instance use =S c= to add all selected entries as children to the visualized
entry, or =S P= to remove the parent relationship of the selected entries. When
you're done and wish to clear the selection use =org-brain-clear-selected=, which
is bound to =S s=.

** Editing text from =org-brain-visualize-mode=

If you have the =polymode= package installed you can edit your entries directly from =org-brain-visualize-mode=. Run =M-x org-brain-polymode= or add =org-brain-polymode= to =org-brain-visualize-mode-hook=. After editing you can use =C-x C-s= (bound to =org-brain-polymode-save=) to save your changes.

** Editing from =org-mode=

You can edit =org-brain= entries directly from =org-mode=. You can use the default
=org-mode= outline structure to define parent/children relationships, but keep in
mind that only entries with an =ID= property will be considered as entries to
=org-brain=; use =M-x org-brain-get-id= to create an =ID= property to the current
=org-mode= headline. Another alternative is to use =M-x org-brain-refile= which will
create the ids for you. You can also create IDs for all headlines in the buffer
with =M-x org-brain-ensure-ids-in-buffer=, and you might find it useful to add
this to =before-save-hook=.

Most of the commands available in =org-brain-visualize= can also be used in
=org-mode= directly, in which case they will operate on the "entry at point". In
other words you can use =M-x org-brain-add-child= directly from =org-mode= in
order to add a child to the =org-brain= entry at point. You may also want to use
the commands =org-brain-goto-<relationsship>= to navigate between entries.

Most of the commands available in =org-brain-visualize-mode= is also bound to the
prefix keymap =org-brain-prefix-map=. You can bind this to a key in =org-mode=, for
instance =C-c b=, and you could then type =C-c b p= to add a parent to the current
entry. Example config: =(define-key org-mode-map (kbd "C-c b")
'org-brain-prefix-map)=.

You may want to create a link to an =org-brain= entry in an =org-mode= file (not
necessarily an =org-brain= file itself). =org-brain= provides several link types
for this purpose. You can use =org-insert-link= (bound to =C-c C-l= in
=org-mode= by default) to insert one of these links. They all have in common
that they, when clicked, will open the =org-brain= entry for editing. When
inserting a link like this, =org-brain= will run completion upon all your
entries.

- =brain:= :: The default kind of link. Just let's you visit another =org-brain=
  entry when clicked. If the variable =org-brain-backlink= is =t= a brain-link will
  also be created as a resource in the link target, linking back to where the
  link was created. If =org-brain-backlink= is set to a string, that string will
  be added as a prefix to the title of the backlink. *Example:* You set
  =org-brain-backlink= to ="<-- "= and create a =brain:= link in =Rabbits= linking to
  =Carrots=. Now a resource with the description =<-- Rabbits= will be created in
  =Carrots=.
- =brain-child:= :: When inserted using =org-insert-link= this will make
                    the linked entry a child to the current =org-brain= entry,
                    upon completion. Keep in mind that this doesn't work if you
                    type the link manually; only by completion through
                    =org-insert-link=.
- =brain-parent:= :: Like =brain-child:= but makes the linked entry a parent of
     the current entry.
- =brain-friend:= :: Like =brain-child:= but adds the linked entry as a friend.
- =brainswitch= :: If you have multiple brains you may want a link which switches to a specific brain and one of its entries. The =brainswitch= link allows for this.

The names of the relationship inserting links (=brain-child=, =brain-parent= and =brain-friend=) can be customized with the variables =org-brain-child-link-name=, =org-brain-parent-link-name=, and =org-brain-friend-link-name=. This customization should be done before loading =org-brain=. If you're using =use-package=, put the customization in the =:init= block.

** Other commands

If you're browsing a file and want to add the file -- or the current line in the file -- as a resource to an entry, you can use =M-x org-brain-add-file-as-resource= or =M-x org-brain-add-file-line-as-resource=. If any of these are run with /universal argument/ =C-u= then add the resources to current/last visualized entry.

** General information

If you try to add a child/parent/friend to an entry which doesn't exist, that
entry will be created. The same is true for many other commands prompting for an
entry, like =org-brain-visualize=. The name of a new entry can be written like
this: =file::headline=. The =headline= will be created as a level one headline in
=file=. If you create a new entry without the headline part, it will by default be
created as a file entry. It is possible to change that though by setting the
variable =org-brain-default-file-parent= to a default file. Let's say you set the
variable to ="brain"= and then add the entry =Bananas= (a non-existent entry). That
would be the same thing as writing =brain::Bananas=.

When adding children, parents, or friends, multiple entries can be added at once
by separating their titles with =org-brain-entry-separator= (which is =;= by
default). For instance =M-x org-brain-add-parent RET music;artists= would add
both =music= and =artists= as parents.

Another available command is =M-x org-brain-agenda=, which can be used to run
=org-agenda= on your =org-brain= files.

** Slashes in file entry titles

When giving a file entry a title, the title can not contain slashes (=/=) if
=org-brain-file-entries-use-title= is =t=.

** Renaming files in =org-brain=

Headline entries use =org-id= to identify themselves, so the headlines can be
manually renamed without worries. File entries, on the other hand, uses the
filename as the identifier. This will cause problems if you try to manually
rename files inside of =org-brain=.

In order to rename a file, use =M-x org-brain-rename-file=.

** Archiving entries

=org-archive= has a problem in =org-brain=: relationships are maintained, even
though the entry should really be removed from the brain. Because of this,
please use =org-brain-archive= instead. This command removes relationships to
the entry in the brain, before archiving it. The command also inserts handy
links to the archived entry's relationships.

** Special tags

You might have a headline which you do not really want as an entry in
=org-brain=. The most basic way to exclude such a headline is simply to not add
an =ID= property to it. However, =org-brain= also provide two tags, which you
can use to tag the headline:

- =:nobrain:= :: This tag excludes the headline, and its subheadings, from your
                 =org-brain= entries. You can change the tag name by modifying
                 =org-brain-exclude-tree-tag=.
- =:childless:= :: This tag does not exclude the headline, but it excludes the
                   subheadings. You can change the tag name by modifying
                   =org-brain-exclude-children-tag=. Works on file entries.

The following tags modifies the kind of information that is shown when an entry
is visualized:

- =:notext:= :: Do not show the entry's text in =org-brain-visualize=. You can
                change the tag name by modifying =org-brain-exclude-text-tag=.
- =:resourceless:= :: Do not show the entry's resources in
     =org-brain-visualize=. You can change the tag name by modifying
     =org-brain-exclude-resources-tag=.
- =:showchildren:= :: By default local child entries aren't shown as text. By
     setting this tag the entry get the entire subtree as text. You can change
     the tag name by modifying =org-brain-show-children-tag=. Works on file
     entries.
- =:nosiblings:= :: You may have an entry with lots of children, and when you visualize one of these children you might not want to see the siblings from this parent. A good example would be if you have an =index= entry or similar. By tagging the parent with =nosiblings= the parent's children will not show siblings from that parent. You can change the tag name by modifying =org-brain-exclude-siblings-tag=.
- :nolocalparent: :: This is similar to =:nosiblings:= but the tagged parent will
     not be shown at all when one of its local children are visualized.

The following tags modify the way how information is shown when an
entry is visualized.

- =:ownline:= :: Make each child of the tagged entry appear on its own
                 line when the tagged entry is visualized. This
                 only affects the tagged entry. It works akin to
                 temporarily setting =org-brain-child-fill-column-sexp=
                 to =0=.

- =:nosort:= :: Display each child of the tagged node in the order the
                children are listed in the file, rather than in the
                sorted order determined by
                =org-brain-visualize-sort-function=. This affects the
                order of the node’s children in both the child list
                (when the tagged node is being visited) and in the
                sibling list (when one of the tagged node’s children
                is being visited).

** Having multiple brains

You can have multiple brains simply by having more than one brain folder. In this way, each folder becomes a separate brain. You can switch between these using =M-x org-brain-switch-brain=. You can also use =brainswitch:= links in =org-mode= to switch brains.

If you run =org-brain-visualize= from inside an org-file in /the root/ of an org-brain directory, =org-brain= will automatically switch to this brain.

** Take note!

=org-brain= creates and uses several headline properties in the =PROPERTIES=
drawer of =org-mode= headlines:

- =BRAIN_PARENTS=
- =BRAIN_CHILDREN=
- =BRAIN_FRIENDS=
- =BRAIN_EDGE_$IDENTIFIER=
- =ID=
- =NICKNAMES=

These properties are also mirrored as file keywords at the top of file entries,
for instance =#+BRAIN_CHILDREN: 00c0f06c-9bd4-4c31-aed0-15bb3361d9a2=.

These properties/keywords are /not meant to be manipulated directly/! If you want
to remove these properties, use the corresponding command instead
(=org-brain-remove-child= or similar). There's currently command to remove
=NICKNAMES= though, so at the moment that has to be done manually.

You might also see that =org-brain= inserts a =RESOURCES= drawer. It is okay to
modify this drawer manually.

The names of the parents/children/friends properties, the prefix for edge
properties and the =RESOURCES= drawer can customized by setting the variables
=org-brain-parents-property-name=, =org-brain-children-property-name=,
=org-brain-friends-property-name=, =org-brain-edge-property-prefix-name= and
=org-brain-resources-drawer-name=, respectively. Of course, after doing any
customization, the property/drawer names of existing brain files have to be
adjusted manually.

** =org-brain= is slow!

If you feel that =org-brain= is slow while indexing your entries (for instance when running =M-x org-brain-visualize=) you can customize =org-brain-file-entries-use-title=, and set it to =nil=. This will display file names when indexing, instead of the file entry's title, which is faster.

Should only the first call of =org-brain-visualize= be slow, an option may be to try persistent caching of the variable =org-brain-headline-cache=. Choose a [[https://www.emacswiki.org/emacs/SessionManagement][session management solution]] that works for you [[https://github.com/thierryvolpiatto/psession][(an option for helm users)]]. A simple and built-in method is to use the savehist package. To do so, you may add the following configuration:

#+begin_src emacs-lisp
(savehist-mode 1)
(setq savehist-additional-variables '(org-brain-headline-cache))
#+end_src

* FAQ
** Wrong number of arguments: (0 . 0), 2

You are probably using the version of =org-mode= that came with your Emacs install, which has a lower version than 9.2. Check the version of org you have installed with =M-x org-version=. See [[https://github.com/Kungsgeten/org-brain/issues/278][Github issue #278]].
* Export to other formats

=org-brain= has no built-in functionality for exporting to other formats. I've
started experimenting with another package named [[https://github.com/Kungsgeten/org-brain-export][org-brain-export]] which might be
merged into =org-brain= in the future. =org-brain-export= is in VERY early stages of
development.
* Helm and Ivy

If you use [[https://github.com/emacs-helm/helm][Helm]] or [[https://oremacs.com/swiper/][Ivy]] you can use the commands =helm-brain= or =counsel-brain= respectively. These allow for visualizing entries, and adding parents/children/friends to the entry at point. They also allow selecting multiple entries.

These commands do not have any keybindings by default.

* Backwards compatibility breaking changes
** 0.7

As of version 0.7 /entry descriptions/ are deprecated. They made visualization slow, and it was quite a hassle to actually write them. The "help echo" text is now used for edge annotations instead.

** 0.4

/This is only relevant if you've been using org-brain before version 0.4/

As of version 0.4 (June 2017) =org-brain= has been rewritten, in order to
increase performance and add more options. Because of this, older setups are
considered obsolete. Prior to 0.4 only files were considered entries, but now
also headlines with an =ID= property are included as entries. Prior to 0.4
=org-brain= was using the =brain:= link and =#+BRAIN_PINNED:= file keyword to
connect files, which was slow due to the need of searching all files for links.
In version 0.4 =org-brain= uses a combination of headline properties, file
keywords, =org-id=, and a data file (=org-brain-data-file=).

No data in old configurations should be lost, but you'll have to update the
connections between entries. This can be done by using =M-x
org-brain-create-relationships-from-links=, but please backup your =org-brain=
directory first.

It is still possible to add children to an entry by using the =brain-child:= link, but
only if the link is inserted with =org-insert-link= (bound to =C-c C-l= in
=org-mode= by default). Linking to specific headlines in a file, via
=brain:filename::*Headline= is *deprecated* and will no longer work, instead you
can convert the headline to an entry and link directly to that.

* Other useful packages

There's some missing functionality in =org-brain=, which you may find useful.
However there are other packages which might improve your =org-brain=
experience. Below are some suggestions (feel free to create an issue or send a
pull request if you have more examples), all of them should be available on
MELPA.

** Chronological entries with =org-expiry=

=org-brain= doesn't add any information on when entries are created, so it is hard
get a list of your entries in chronological order. I've managed to use
=org-expiry= (part of =org-plus-contrib=) to add a =CREATED= property to all =org-brain=
headline entries. Then we can use =org-agenda= to show the entries in
chronological order.

#+BEGIN_SRC emacs-lisp
  ;; Setup org-expiry and define a org-agenda function to compare timestamps
  (require 'org-expiry)
  (setq org-expiry-inactive-timestamps t)
  (defun org-expiry-created-comp (a b)
    "Compare `org-expiry-created-property-name' properties of A and B."
    (let ((ta (ignore-errors
                (org-time-string-to-seconds
                 (org-entry-get (get-text-property 0 'org-marker a)
                                org-expiry-created-property-name))))
          (tb (ignore-errors
                (org-time-string-to-seconds
                 (org-entry-get (get-text-property 0 'org-marker b)
                                org-expiry-created-property-name)))))
      (cond ((if ta (and tb (< ta tb)) tb) -1)
            ((if tb (and ta (< tb ta)) ta) +1))))

  ;; Add CREATED property when adding a new org-brain headline entry
  (add-hook 'org-brain-new-entry-hook #'org-expiry-insert-created)

  ;; Finally add a function which lets us watch the entries chronologically
  (defun org-brain-timeline ()
    "List all org-brain headlines in chronological order."
    (interactive)
    (let ((org-agenda-files (org-brain-files))
          (org-agenda-cmp-user-defined #'org-expiry-created-comp)
          (org-agenda-sorting-strategy '(user-defined-down)))
      (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))
#+END_SRC

Now we can use =org-brain-timeline= to view entries in chronological order (newest
first).

** [[https://github.com/rexim/org-cliplink][org-cliplink]]

#+BEGIN_QUOTE
A simple command that takes a URL from the clipboard and inserts an org-mode link with a title of a page found by the URL into the current buffer.
#+END_QUOTE

Here's a command which uses =org-cliplink= to add a link from the clipboard as an =org-brain= resource. It guesses the description from the URL title. Here I've bound it to =L= in =org-brain-visualize=.

#+BEGIN_SRC emacs-lisp
  (defun org-brain-cliplink-resource ()
    "Add a URL from the clipboard as an org-brain resource.
  Suggest the URL title as a description for resource."
    (interactive)
    (let ((url (org-cliplink-clipboard-content)))
      (org-brain-add-resource
       url
       (org-cliplink-retrieve-title-synchronously url)
       t)))

  (define-key org-brain-visualize-mode-map (kbd "L") #'org-brain-cliplink-resource)
#+END_SRC

** [[https://github.com/noctuid/link-hint.el][link-hint]]

#+BEGIN_QUOTE
link-hint.el is inspired by the link hinting functionality in vim-like browsers
and browser plugins such as pentadactyl. It provides commands for using avy to
open or copy "links."
#+END_QUOTE

After installing =link-hint= you could bind =link-hint-open-link= to a key, and
use it in =org-brain-visualize-mode=. If you only want to use =link-hint= in
=org-brain-visualize-mode=, you could add the following to your init-file:

#+BEGIN_SRC emacs-lisp
  (define-key org-brain-visualize-mode-map (kbd "C-l") #'link-hint-open-link)
#+END_SRC

** [[http://www.gnuvola.org/software/aa2u/][ascii-art-to-unicode]]

#+BEGIN_QUOTE
Converts simple ASCII art line drawings in the region of the current buffer to
Unicode.
#+END_QUOTE

=ascii-art-to-unicode= is useful if you want =org-brain-visualize-mode= to look
a bit nicer. After installing, add the following to your init-file:

#+BEGIN_SRC emacs-lisp
  (defface aa2u-face '((t . nil))
    "Face for aa2u box drawing characters")
  (advice-add #'aa2u-1c :filter-return
              (lambda (str) (propertize str 'face 'aa2u-face)))
  (defun aa2u-org-brain-buffer ()
    (let ((inhibit-read-only t))
      (make-local-variable 'face-remapping-alist)
      (add-to-list 'face-remapping-alist
                   '(aa2u-face . org-brain-wires))
      (ignore-errors (aa2u (point-min) (point-max)))))
  (with-eval-after-load 'org-brain
    (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))
#+END_SRC

** [[https://github.com/domtronn/all-the-icons.el][all-the-icons]]

#+BEGIN_QUOTE
A utility package to collect various Icon Fonts and propertize them within Emacs.
#+END_QUOTE

After installing =all-the-icons= you could decorate the resources in =org-brain=, by using
=org-brain-after-resource-button-functions=. Here's a small example:

#+BEGIN_SRC emacs-lisp
  (defun org-brain-insert-resource-icon (link)
    "Insert an icon, based on content of org-mode LINK."
    (insert (format "%s "
                    (cond ((string-prefix-p "brain:" link)
                           (all-the-icons-fileicon "brain"))
                          ((string-prefix-p "info:" link)
                           (all-the-icons-octicon "info"))
                          ((string-prefix-p "help:" link)
                           (all-the-icons-material "help"))
                          ((string-prefix-p "http" link)
                           (all-the-icons-icon-for-url link))
                          (t
                           (all-the-icons-icon-for-file link))))))

    (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)
#+END_SRC

You could also use =all-the-icons= to add icons to entry [[https://orgmode.org/manual/Categories.html][categories]]. For instance if you have two categories named /computers/ and /books/ which you want icons for:

#+BEGIN_SRC emacs-lisp
  (setq org-agenda-category-icon-alist
        `(("computers" ,(list (all-the-icons-material "computer")) nil nil :ascent center)
          ("books" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)))
#+END_SRC

** [[http://jblevins.org/projects/deft/][deft]]

#+BEGIN_QUOTE
An Emacs mode for quickly browsing, filtering, and editing directories of plain
text notes, inspired by Notational Velocity.
#+END_QUOTE

After installing =deft=, you can add the function below to your init-file.

#+BEGIN_SRC emacs-lisp
  (defun org-brain-deft ()
    "Use `deft' for files in `org-brain-path'."
    (interactive)
    (let ((deft-directory org-brain-path)
          (deft-recursive t)
          (deft-extensions '("org")))
      (deft)))
#+END_SRC

** [[https://github.com/alphapapa/helm-org-rifle][helm-org-rifle]]

#+BEGIN_QUOTE
It searches both headings and contents of entries in Org buffers, and it
displays entries that match all search terms, whether the terms appear in the
heading, the contents, or both.
#+END_QUOTE

After installing =helm-org-rifle=, you can add the function below to your
init-file.

#+BEGIN_SRC emacs-lisp
  (defun helm-org-rifle-brain ()
    "Rifle files in `org-brain-path'."
    (interactive)
    (let ((helm-org-rifle-close-unopened-file-buffers nil))
      (helm-org-rifle-directories (list org-brain-path))))

  (defun helm-org-rifle-open-in-brain (candidate)
    (-let (((buffer . pos) candidate))
      (with-current-buffer buffer
        (goto-char pos)
        (org-brain-visualize-entry-at-pt))))

  (add-to-list 'helm-org-rifle-actions
               (cons "Show entry in org-brain" 'helm-org-rifle-open-in-brain)
               t)
#+END_SRC

** [[https://github.com/weirdNox/org-noter][org-noter]]

#+BEGIN_QUOTE
Org-noter's purpose is to let you create notes that are kept in sync when you scroll through the [PDF etc] document
#+END_QUOTE

Thanks to [[https://github.com/rosetree][rosetree]] for providing this tip! After installing =org-noter=, add the following to your init-file:

#+BEGIN_SRC emacs-lisp
(add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
(defun org-brain-open-org-noter (entry)
    "Open `org-noter' on the ENTRY.
If run interactively, get ENTRY from context."
    (interactive (list (org-brain-entry-at-pt)))
    (org-with-point-at (org-brain-entry-marker entry)
      (org-noter)))
#+END_SRC

=org-brain-open-org-noter= will run =org-noter= on the current entry. This lets you save your PDF notes in =org-brain=, so you can link to them from other entries etc. It can be a good idea to add this command to =org-brain-visualize=, like this:

#+BEGIN_SRC emacs-lisp
  (define-key org-brain-visualize-mode-map (kbd "\C-c n") 'org-brain-open-org-noter)
#+END_SRC

** [[https://github.com/scallywag/org-board][org-board]]
#+BEGIN_QUOTE
org-board is a bookmarking and web archival system for Emacs Org mode, building
on ideas from Pinboard. It archives your bookmarks so that you can access them
even when you're not online, or when the site hosting them goes down.
#+END_QUOTE

* Similar packages

The Emacs Wiki has an article about [[https://www.emacswiki.org/emacs/WikiModes][wiki modes in Emacs]].

** [[https://github.com/jethrokuan/org-roam][org-roam]]

#+BEGIN_QUOTE
Org-roam is a Roam replica built on top of the all-powerful Org-mode.

Org-roam is a solution for effortless non-hierarchical note-taking with
Org-mode. With Org-roam, notes flow naturally, making note-taking fun and easy.
Org-roam should also work as a plug-and-play solution for anyone already using
Org-mode for their personal wiki.

Org-roam aims to implement the core features of Roam, leveraging the mature
ecosystem around Org-mode where possible. Eventually, we hope to further
introduce features enabled by the Emacs ecosystem.
#+END_QUOTE

** [[https://github.com/l3kn/org-zettelkasten][Org Zettelkasten]]

#+begin_quote
An opinionated setup for managing large collections of interlinked org files.
#+end_quote

** [[https://github.com/caiorss/org-wiki][org-wiki]]

#+BEGIN_QUOTE
Org-wiki is a org-mode extension that provides tools to manage and build
personal wiki or desktop wiki where each wiki page is a org-mode file.
#+END_QUOTE

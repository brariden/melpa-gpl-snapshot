# ruby-factory-mode

[![MELPA](http://melpa.org/packages/ruby-factory-badge.svg)](http://melpa.org/#/ruby-factory)

Emacs minor mode for Ruby test object generation libraries. Currently supports
[factory_girl](https://github.com/thoughtbot/factory_girl) and [Fabrication](https://github.com/paulelliott/fabrication)
and only under Rails (for now).

## Requirements

* [inflections](https://github.com/eschulte/jump.el)
* If you want to use the snippets you'll need [YASnippet](https://github.com/capitaomorte/yasnippet)  `v0.8.1` or greater

## Usage

The preferred method of installation is via [a package](http://www.emacswiki.org/emacs/InstallingPackages). ruby-factory-mode is available on [MELPA](http://melpa.org).

To do things the old fashioned way:

```elisp
(add-to-list 'load-path "/location/of/ruby-factory")
(require 'ruby-factory)
```

To enable the mode automatically add a `ruby-mode-hook`:
```elisp
(add-hook 'ruby-mode-hook 'ruby-factory-mode)
```

If you're using RSpec it's recommended to use this with [rspec-mode](https://github.com/pezra/rspec-mode).

### Key Bindings

<kbd>C-c , j</kbd> - **J**ump to the current buffer's model or factory

### Snippets

#### factory_girl

Binding | Snippet
--------|------------------------------------|
`aft`   | `after :hook do |model| ... end`   |
`aftb`  | `after :build do |model| ... end`  |
`aftc`  | `after :create do |model| ... end` |
`afts`  | `after :stub do |model| ... end`   |
`bef`   | `before :create do |model| ... end`|
`fg`    | `FactoryGirl.define do ... end`    |
`fac`   | `factory :model do ... end`        |
`initw` | `initialize_with { ... }`          |
`seq`   | `sequence(:attribute) { |i| ... }` |
`trai`  | `trait :attribute do ... end`      |
`tran`  | `transient do ... end`             |


#### Fabrication

Binding | Snippet
--------|-----------------------------------------|
`aft`   | `after_hook do |model| ... end`         |
`aftb`  | `after_build do |model| ... end`        |
`aftc`  | `after_create do |model| ... end`       |
`aftv`  | `after_validation do |model| ... end`   |
`bef`   | `before_hook do |model| ... end`        |
`befc`  | `before_create do |model| ... end`      |
`befs`  | `before_save do |model| ... end`        |
`befv`  | `before_validation do |model| ... end`  |
`fab`   | `Fabricator :model do ... end`          |
`initw` | `initialize_with { ... }`               |
`seq`   | `sequence(:attribute) { |i| ... }`      |
`tran`  | `transient :attribute`                  |

## See Also

* [rspec-mode](https://github.com/pezra/rspec-mode)

## TODO

* Non-Rails projects
* More sophisticated snippets

## Author

Skye Shaw (skye.shaw ~AT~ gmail.com)

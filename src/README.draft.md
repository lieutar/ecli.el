# ecli.el

Emacs Lisp で、CLI アプリケーションを作成するための仕組みを提供します。
作ることのできるアプリケーションは、
特定のパターンの名前をもった関数によりそのロジックが決定され、
そのドキュメントによって、ヘルプメッセージが自動的に作成されます。

Provide a mechanism for creating CLI applications in Emacs Lisp.
The applications you can create
have their logic determined by functions named in specific patterns,
and their documentation automatically creates help messages.

## Purpose

Emacs はあくまでテキストエディタであり、如何に Emacs Lisp 
が強力な言語であっても、一般的なアプリケーションを作成する目的には向いていません。

しかし、Emacs 自身の環境を扱う処理や、Emacs Lisp モジュールの開発環境、eld ファイル
に対する処理など、Emacs 自身を扱うものについてはそれらへの詳細な API を持つ Emacs Lisp 
での実装が向いている領域です。

`ecli.el` はそういった領域の処理を CLI で行うためのコマンドライン引数処理、
およびヘルプの作成などに主眼を置いたフレームワークです。

Emacs is a text editor, and no matter how powerful Emacs Lisp is, it is not 
suitable for creating general applications.

However, when it comes to processing that deals with Emacs itself, 
such as processing that deals with the Emacs environment, 
the development environment for Emacs Lisp modules, 
and processing for eld files, these are areas that are best implemented in 
Emacs Lisp, which has a detailed API for these.

`ecli.el` is a framework that focuses on command line argument processing to 
perform such processing in a CLI, and on creating help.

## Basic Usage as a CLI app framework

ここでは [サンプルアプリケーション](samples/taskman) を例に解説をします。

This section will explain using the [sample application](samples/taskman) 
as an example.

### Define Functions to process the app's logic

まずはじめに、アプリケーションの為のロジックを記述した関数を定義します。
各関数はユーザが定義した prefix と、フレームワークの定義した prefix 、
そしてその内容を併せた名前を持ちます。

以下の例では、ユーザ定義の prefix は `taskman-` 、フレームワーク定義の prefix は
`sub-` ないし `opt-` ですね。

ここでは `taskman-opt-` はオプション、 `taskman-sub-` はサブコマンドの処理を行います。

#### オプション定義

`*opt-` prefix を持つオプション処理関数は未処理の引数リストと、
app spec を引数として受け取り、処理をします。

app spec の実態はフレームワークが用いる plist であり `ecli-process-args` 
の引数と、アプリケーションのロジックの為に定義された関数を元に作成されます。
その内容については、基本

    
``` emacs-lisp
(defun taskman-opt--help:-h (args app-spec)
  "Show help message.
"
  (funcall (plist-get app-spec :help))
  (kill-emacs 0))

(defun taskman-opt--version:-v (args app-spec)
  "Show version information.
"
  (princ (format "taskman %s\n" taskman-VERSION))
  (kill-emacs 0))
```

このように、オプション処理中に直ちに処理を終了することもできますね。

オプション処理関数は、アプリケーションに影響を与えるデータ `:value` と、
自身が処理できなかった引数のリスト `:args` を返します。

`:value` の形は何でも良いですが、`(:keyword . value )` のような形のリストにすると、
サブコマンド処理関数は結果を alist として利用できるので推奨されます。

ただ、以下の例のようにグローバル変数を更新して `:value` 
としては何も返さないこともできますね。

``` emacs-lisp

(defun taskman-opt--dbfile:-f (args app-spec)
  "Specify path to the database file.
"
  (let ((db-file (cadr args)))
    (setq taskman-db-file db-file))
  (list :value nil :args (ccdr args)))
```

なお、オプション定義関数の documentation 
はそのままヘルプメッセージを構築するために利用されます。

#### サブコマンド定義

`*sub-` prefix を持つサブコマンド処理関数は、ecli が処理できなかった引数リストと、
オプション処理関数でも用いた app-spec を引数として受け取りますが、ここではさらに、
処理済みオプションから得られた値のリストが `:options` に追加されています。




``` emacs-lisp

(defun taskman-sub-remove (args &rest app-spec)
  "taskman remove <task-id>
Remove a task pointed by the task-id
"
  (unless (= 1 (length args))
    (funcall (plist-get app-spec :error)
             "taskman remove receives only one argument."))
  (let* ((db   (taskman-read))
         (id   (car args))
         (slot (taskman-get-slot db id)))
    (if slot
        (progn
          (taskman-write (cons (car db)
                               (--filter (not (eq (car it) id))
                                         (cdr db))))
          (princ "Removed %s\n" id))
      (funcall (plist-get app-spec :error)
               (format "Task %s isn't exists." id)))))


```

ecli ではサブコマンドが無い場合のデフォルト関数を定義することもできます。
これにより、サブコマンドを持たないアプリケーションを作成することもできます。

``` emacs-lisp
(defun myapp-default (args &rest opts)
   )
```

デフォルト関数が指定されておらず、アプリケーションにサブコマンドの指定もない場合 ecli 
はヘルプメッセージを出力して終了します。

また、規定のサブコマンドをデフォルト関数として指定することもできます。
デフォルト関数はサブコマンドと同じ API で定義できるので、
単にエイリアスをするだけでも勿論良いですし、後に述べる関数 `ecli-process-args` 
の `:default` オプションとして関数を渡すこともできます。

### Build your app's spec and use it

関数 `ecli-process-args` は、与えられた定義に従い引数を処理します。
CLI アプリケーションの場合は、`command-line-args-left` を処理の対象とするように、
以下のようにします。

``` emacs-lisp
(let ((args command-line-args-left))
  (setq command-line-args-left nil)
  (ecli-process-args args :prefix 'myapp-))
```

この例では、`command-line-args-left` を消費し切るために、局所変数に退避した後、
`nil` で上書きしており、続く引数を Emacs 自身に処理させることを防いでいます。

### And then run your app

``` sh
emacs --script myapp.el
```

**with shell script**
``` sh
#! /usr/bin/env sh
self=`realpath $0`
here=`basename $self`
emacs --script $here/myapp.el "$@"
```

普通は、ランチャスクリプトを介した起動が簡単です。
ただし、上記のスクリプトには幾つかの制約事項があります。
それは、 `--help` や `--version` といったオプションは emacs 自身が処理をしてしまって、
スクリプトに渡らないという問題があるからです。

そのため、例のようにあくまで bourne shell を用いる場合は、`--help` や `--version`
といったオプションを使わず、 `--usage` や `--show-version` 
といった異なるオプションを使うようにするか、もしくは bash や python 、 perl 
などといったより高度な処理の出来るスクリプトによって `@--help` `@--version`
という形にオプションを加工することです。

ecli は `@--long-option` や `@-o` なども `--long-option` や `-o` 
として処理をするようにデザインされています。

bash を使う例は以下です。

``` bash
args=()
for arg in $@; do
    case "$arg" in
        -*)
            arg="@$arg"
            ;;
    esac
    args+=("$arg")
done

emacs --script $here/myapp.el "${args[@]}"

```

なお、ecli を含めた依存モジュールのロードの為のロジックは、
ランチャが構築するコマンドライン引数や、起動される emacs lisp
スクリプトの中に記述される必要があります。

## Build installer for your app

``` bash
emacs --batch -l path/to/ecli.el -f ecli-build-installer\
-a melpa \
-a foo=https://you.github.io/foo/packages \
-d foo \
-d bar \
--default-prefix ~/.foo \
--lancher https://raw.githubusercontent.com/you/foo/refs/heads/main/bin/foo \
--app-el  https://raw.githubusercontent.com/you/foo/refs/heads/main/bin/foo.el \
> install
```

This command emits installer script (written in bash).
Options are:

  - `--archive`    `-a` archive-name=archive-path
  - `--depends-on` `-d` modules 
  - `--default-prefix` `-P` default path to be installed
  - `--launcher`   `-L` URL to launcher script
  - `--app-el`     `-E` URL to the app written in Emacs Lisp

And then, the script will be able to distribute like:

``` bash
curl -fsSL https://raw.githubusercontent.com/you/foo/refs/heads/main/install |bash

```

If you want to customize the path to be installed, you can use 
the `PREFIX` environment variable for this purpose.

``` bash
curl -fsSL https://raw.githubusercontent.com/you/foo/refs/heads/main/install \
    |PREFIX=~/apps/foo bash
```

If you manage local melpa mirror or other customized ELPA's you can write it on
some .eld file on some path you managed.

``` emacs-lisp
(("melpa" . "~/work/emacs/melpa-mirror/packages")
 ("foo"   . "~/work/emacs/my-custom-elpa/packages"))
```

And then, you can pass it like following command line:

``` bash
curl -fsSL https://raw.githubusercontent.com/you/foo/refs/heads/main/install \
    |PREFIX=~/apps/foo ARCHIVES_ALIST_ELD=~/work/emacs/local-elpas.eld bash
```

## Helper functions for your app

### function `ecli-help`
``` emacs-lisp
(ecli-help app-spec)
(kill-emacs 0)
```

### function `ecli-help-message`
``` emacs-lisp
(setq msg (ecli-help-message app-spec))
```


### function `ecli-error`
``` emacs-lisp
(ecli-error app-spec "error-message")
```

### function `ecli-put-option-value`
``` emacs-lisp
(ecli-put-option-value app-spec :option-foo value)
```

### function `ecli-push-option-value`

``` emacs-lisp
(ecli-push-option-value app-spec :option-foo value)
```


### function `ecli-get-option-value`

``` emacs-lisp
(setq value (ecli-get-option-value app-spec :option-foo))
```

### macro `ecli-setup-app-context`

``` emacs-lisp
(ecli-setup-app-context)
```
インストールされるアプリケーションは以下のような構造になることが想定されています。
```
$PREFIX -+- bin/
         |  |
         |  `-- launcher.sh
         |
         +- share/lisp/
         |  |
         |  +-- app.el
         |  |
         |  `-- eclie.el
         |
         `- cache/elpa/
            |
            +-- depmod-a-VVVVVV
            |
            +-- depmod-b-VVVVVV
            |
            .
            .
            .
```

laucher.sh が直接起動されるファイル、 app.el がアプリケーションを記述したファイル、
share/elpa/ 以下が依存モジュールですね。

`ecli-setup-app-context` はこの依存モジュール群へのロードパスを通します。

## 制限事項 // Restrictions

Emacs Lisp のバッチプロセスは、その仕様上インタラクションを定義できません。
もしインタラクションが必要な場合はランチャスクリプトの方で予め処理を行い、
その結果はオプション引数や環境変数を通じて elisp に渡す必要があります。

//

Emacs Lisp batch processes cannot define interactions due to their 
specifications.
If interactions are required, they must be processed in advance in the 
launcher script,
and the results must be passed to elisp via option arguments or environment 
variables.

  
## License

{{license}}

## Copyright

{{copyright}}


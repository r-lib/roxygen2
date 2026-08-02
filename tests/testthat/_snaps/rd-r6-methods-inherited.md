# r6_inherited_documentation_display = 'single' renders a single pointer to the immediate parent

    Code
      cat(format(inherited), sep = "\n")
    Output
      \if{html}{\out{<p>+ inherited public methods from <code>pkg::A</code>.</p>}}

# r6_inherited_documentation_display = 'grouped' (the default) groups methods by originating ancestor

    Code
      cat(format(inherited), sep = "\n")
    Output
      \if{html}{\out{<details><summary>+ inherited public methods from pkg::B</summary>
      <ul>
        <li><code>pkg::B$b1()</code></li>
      </ul>
      </details>
      <details><summary>+ inherited public methods from pkg::A</summary>
      <ul>
        <li><code>pkg::A$shared()</code></li>
        <li><code>pkg::A$only_a()</code></li>
      </ul>
      </details>}}

# r6_inherited_documentation_display = 'grouped' skips ancestors with no contributed methods

    Code
      cat(format(inherited), sep = "\n")
    Output
      \if{html}{\out{<details><summary>+ inherited public methods from pkg::A</summary>
      <ul>
        <li><code>pkg::A$only_a()</code></li>
      </ul>
      </details>}}

# r6_inherited_documentation_display = 'original' restores the pre-8.1.0 flat list

    Code
      cat(format(inherited), sep = "\n")
    Output
      \if{html}{\out{<details open><summary>Inherited methods</summary>
      <ul>
        <li><code>pkg::A$shared()</code></li>
        <li><code>pkg::B$b1()</code></li>
        <li><code>pkg::A$only_a()</code></li>
      </ul>
      </details>}}


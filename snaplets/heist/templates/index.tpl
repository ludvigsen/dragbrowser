<apply template="base">

  <ifLoggedIn>
    <!--
    <p>
      This is a simple demo page served using
      <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
      and the <a href="http://snapframework.com/">Snap</a> web framework.
    </p>

    <p>Congrats!  You're logged in as '<loggedInUser/>'</p>
    -->
    <p><a href="/logout">Logout</a></p>
    <p><a href="/new_user">Create a new user</a></p>
    <div id="main" />

    <div id="hiddenFooter">
        <div id="progressBar" class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100">
            <span class="sr-only"></span>
        </div>
    </div>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>

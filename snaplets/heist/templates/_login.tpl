<!--<h1>Snap Example App Login</h1>-->

<div class="container login">
<div class="row">
<div class="offset4 span4">
<p><loginError/></p>

<bind tag="postAction">/login</bind>
<bind tag="submitText">Login</bind>
<apply template="userform"/>

<p>Don't have a login yet? <a href="/new_user">Create a new user</a></p>
</div>
</div>
</div>

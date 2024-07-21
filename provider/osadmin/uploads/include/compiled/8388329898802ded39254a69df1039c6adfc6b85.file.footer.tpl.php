<?php /* Smarty version Smarty-3.1.15, created on 2014-11-13 14:57:35
         compiled from "E:\software\wamp\www\osadmin\uploads\include\template\footer.tpl" */ ?>
<?php /*%%SmartyHeaderCode:1829954643a8cdc3447-86827877%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '8388329898802ded39254a69df1039c6adfc6b85' => 
    array (
      0 => 'E:\\software\\wamp\\www\\osadmin\\uploads\\include\\template\\footer.tpl',
      1 => 1415861817,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '1829954643a8cdc3447-86827877',
  'function' => 
  array (
  ),
  'version' => 'Smarty-3.1.15',
  'unifunc' => 'content_54643a8cdd6cc8_35587701',
  'has_nocache_code' => false,
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_54643a8cdd6cc8_35587701')) {function content_54643a8cdd6cc8_35587701($_smarty_tpl) {?>

					<footer>
                    <!--
                        <hr>
                        <p class="pull-right">A <a href="http://localhost/" target="_blank">Basic Backstage Management System for China Only.</a> by <a href="http://localhost" target="_blank">anything</a>.</a>】</p>

                        <p>&copy; 2013 <a href="http://localhost" target="_blank">anything</a></p>
                        -->
                    </footer>
				</div>
			</div>
		</div>
    <script src="<?php echo @constant('ADMIN_URL');?>
/assets/lib/bootstrap/js/bootstrap.js"></script>

<!--- + -快捷方式的提示 --->

<script type="text/javascript">

alertDismiss("alert-success",3);
alertDismiss("alert-info",10);

listenShortCut("icon-plus");
listenShortCut("icon-minus");
doSidebar();
</script>
  </body>
</html>
<?php }} ?>
